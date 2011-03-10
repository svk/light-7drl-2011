(require :asdf)
(asdf:operate 'asdf:load-op :cffi)
(asdf:operate 'asdf:load-op :tcod)

(defpackage :light-7drl
  (:use :common-lisp :cffi :tcod))
(in-package :light-7drl)

(cffi:define-foreign-library libtcod
    (t (:default "libtcod")))

(defun terminate ()
  (handler-case
      (progn
	(defcfun "TCOD_sys_term" :void)
	(TCOD-sys-term))
    (sb-kernel::undefined-alien-function-error ())))

(defun load-libraries ()
  (cffi:load-foreign-library 'libtcod :search-path "libtcod.so")
  (setf tcod:*root* (null-pointer)))

(defun close-libraries ()
  (cffi:close-foreign-library 'libtcod))

(defconstant +wall-glyph+ 219)
(defconstant +player-glyph+ (char-code #\@))
(defconstant +floor-glyph+ (char-code #\ ))

(defconstant +screen-width+ 60)
(defconstant +screen-height+ 40)

(defparameter *tileset-file* "my-tiles.png")

(defconstant +ui-top-lines+ 3)
(defconstant +ui-bottom-lines+ 1)

(defparameter *game-map* nil)
(defparameter *game-player* nil)
(defparameter *game-running* nil)
(defparameter *game-text-buffer* nil)

(defparameter *game-initialized* nil)

(defparameter *debug-level* 50)

(defparameter *game-input-hooks* nil)

(defparameter *game-torch* nil)
(defparameter *game-brazier* nil)
(defparameter *game-creatures* nil)

(defconstant +light-visibility-threshold+ (/ 1 255))
(defconstant +minimum-fg-light+ 0.3)
(defconstant +minimum-touched-light+ 0.1)

(defparameter *cheat-lightall* nil)

(defparameter *directions* (list
			     (cons -1 -1) (cons 0 -1) (cons 1 -1)
			     (cons -1 0)              (cons 1 0)
			     (cons -1 1)  (cons 0 1)  (cons 1 1)))


(defun light-half-life (steps)
  "Calculate the factor of (exponential) decay required for light intensity to halve at a distance of steps."
  (/ (- (log 2)) steps))

(defparameter *universal-light-half-life* (light-half-life 4))

(defmacro debug-print (level &rest rest)
  (if (<= level *debug-level*)
      `(format t ,@rest)
      nil))

(defstruct appearance
  glyph
  (foreground-colour '(255 255 255))
  (background-colour nil))

(defstruct tile
  appearance
  opaque
  walkable
  (visible nil)
  (creature nil)
  (items nil)
  (lighting 0))

(defstruct creature
  appearance
  name
  hp
  max-hp
  (ai nil)
  (tile nil)
  (xy nil))


(defun apply-to-all-tiles (map f)
  (let ((map-width (car (array-dimensions map)))
	(map-height (cadr (array-dimensions map))))
    (dotimes (x map-width)
      (dotimes (y map-height)
	(funcall f (aref map x y))))))

(defun apply-to-all-tiles-xy (map f)
  (let ((map-width (car (array-dimensions map)))
	(map-height (cadr (array-dimensions map))))
    (dotimes (x map-width)
      (dotimes (y map-height)
	(funcall f (aref map x y) x y)))))

(defun apply-to-all-xy (f)
  (let ((map-width (car (array-dimensions *game-map*)))
	(map-height (cadr (array-dimensions *game-map*))))
    (dotimes (x map-width)
      (dotimes (y map-height)
	(funcall f x y)))))

(defun clear-tile-light (tile)
  (setf (tile-lighting tile) 0))

(defun clear-lighting ()
  (apply-to-all-tiles *game-map* #'clear-tile-light))

(defun distance (ax ay bx by)
  (let ((dx (- ax bx))
	(dy (- ay by)))
    (sqrt (+ (* dx dx) (* dy dy)))))

(defstruct light-source
  x
  y
  intensity
  (cached-fov nil))

(defun extract-fov (fov-map)
  (let ((rv (make-array (array-dimensions *game-map*))))
    (apply-to-all-xy #'(lambda (x y)
			 (setf (aref rv x y)
			       (tcod:map-is-in-fov? fov-map x y))))
    rv))

(defun move-light-source (source x y)
  (setf (light-source-cached-fov source) nil
	(light-source-x source) x
	(light-source-y source) y))

(defun add-light-from-source (source fov-map &optional (factor *universal-light-half-life*))
  ;; Physically speaking I'm pretty sure this is supposed to be inverse-square,
  ;; but that just looks terrible, both gameplay-wise and aesthetically. So
  ;; light in my game will decay exponentially with distance.
  (let* ((cx (light-source-x source))
	 (cy (light-source-y source))
	 (intensity (light-source-intensity source))
	 (fov (setf (light-source-cached-fov source)
		    (or (light-source-cached-fov source)
			(extract-fov
			 (progn
			   (tcod:map-compute-fov fov-map
					       cx
					       cy
					       1000
					       t
					       :fov-shadow)
			   fov-map))))))
    (apply-to-all-tiles-xy
     *game-map*
     #'(lambda (tile x y)
	 (unless (not (aref fov x y))
	   (setf (tile-lighting tile)
		 (+ (tile-lighting tile)
		    (* intensity (exp (* factor (distance x y cx cy)))))))))))
  
(defun creature-can-walk? (creature tile)
  (declare (ignore creature))
  (and
   tile
   (tile-walkable tile)
   (not (tile-creature tile))))

(defun creature-x (creature)
  (car (creature-xy creature)))

(defun creature-y (creature)
  (cdr (creature-xy creature)))

(defun tile-at (map x y)
  (unless (not (array-in-bounds-p map x y))
    (aref map x y)))

(defun try-move-creature (map creature dx dy)
  (let* ((xy (creature-xy creature))
	 (x (car xy))
	 (y (cdr xy))
	 (xp (+ x dx))
	 (yp (+ y dy))
	 (tile (tile-at map xp yp)))
    (unless (not (creature-can-walk? creature tile))
      (set-creature-position creature map xp yp)
      tile)))

(defun remove-creature-from-map (map creature)
  (unless (null (creature-tile creature))
    (setf (tile-creature (creature-tile creature)) nil))
  (setf (creature-tile creature) nil
	(creature-xy creature) nil))

(defun set-creature-position (creature map x y)
  (remove-creature-from-map map creature)
  (setf (tile-creature (tile-at map x y)) creature
	(creature-xy creature) (cons x y)
	(creature-tile creature) (tile-at map x y)))

(defun spawn-creature (creature map)
  (let* ((xy (select-random 
	     (remove-if-not #'(lambda (xy) (creature-can-walk? creature (aref map (car xy) (cdr xy))))
			    (find-walkables map))))
	 (x (car xy))
	 (y (cdr xy)))
    (debug-print 50 "Spawning creature ~a on ~a.~%" creature xy)
    (set-creature-position creature map x y)
    (push creature *game-creatures*)
    creature))

(defun tick-creatures (list)
  (dolist (creature list)
    (unless (null (creature-ai creature))
      (funcall (creature-ai creature) creature))))

(defun make-wall-tile ()
  (make-tile :appearance (make-appearance :glyph +wall-glyph+
					  :foreground-colour '(255 64 0)
					  :background-colour '(0 0 0))
	     :opaque t
	     :walkable nil))

(defun make-floor-tile ()
  (make-tile :appearance (make-appearance :glyph +floor-glyph+
					  :foreground-colour '(255 255 255)
					  :background-colour '(255 255 64))
	     :opaque nil
	     :walkable t))

(defun overlay-appearances (above below)
  (unless (null (appearance-background-colour above))
    (return-from overlay-appearances above))
  (make-appearance :glyph (appearance-glyph above)
		   :foreground-colour (appearance-foreground-colour above)
		   :background-colour (appearance-background-colour below)))

(defun appearance-at (tile)
  (unless (not (tile-creature tile))
    (return-from appearance-at
      (overlay-appearances
       (creature-appearance (tile-creature tile))
       (tile-appearance tile))))
;;  (unless (not (tile-items tile))
;;    (return-from appearance-at (item-appearance (car (tile-items tile)))))
  (tile-appearance tile))

(defun create-game-map (width height)
  (let ((rv (make-array (list width height))))
    (dotimes (x width)
      (dotimes (y height)
	(setf (aref rv x y)
	      (if (or (= x 0)
		      (= y 0)
		      (= (+ 1 x) width)
		      (= (+ 1 y) height))
		  (make-floor-tile)
		  (make-floor-tile)))))
    rv))

(defun create-game-map-test (width height)
  (let ((map (create-game-map width height)))
    (dotimes (x width)
      (dotimes (y height)
	(setf (aref map x y) (if (and (= 0 (random 8))
				      (< 10 (distance 2 8 x y)))
				 (make-wall-tile)
				 (make-floor-tile)))))
    map))

(defun find-walkables (map)
  (do* ((width (car (array-dimensions map)))
	(height (cadr (array-dimensions map)))
	(x 0 (+ x 1))
	(rv nil))
       ((>= x width) rv)
    (dotimes (y height)
      (unless (not (tile-walkable (aref map x y)))
	(push (cons x y) rv)))))

(defun select-random (list)
  (nth (random (length list)) list))

(defun find-random-walkable (map)
  (select-random (find-walkables map)))

(defun set-tcod-opacity-map (map fov-map)
  (let ((width (car (array-dimensions map)))
	(height (cadr (array-dimensions map))))
    (dotimes (x width)
      (dotimes (y height)
	(tcod:map-set-properties fov-map
				 x
				 y
				 (not (tile-opaque (aref map x y)))
				 nil)))))

(defun game-loop ()
  (do* ((*game-running* t)
	(map-width (car (array-dimensions *game-map*)))
	(map-height (cadr (array-dimensions *game-map*)))
	(fov-map (tcod:map-new map-width map-height))
	(terrain-updated t)
	(fov-updated t)
	(player-last-xy nil)
	(tcod-black (tcod:compose-colour 0 0 0))
	(tcod-white (tcod:compose-colour 255 255 255)))
       ((not *game-running*))
    (debug-print 2000 "Looping.~%")
    (unless (eq (creature-xy *game-player*) player-last-xy)
      (setf fov-updated t)
      (setf player-last-xy (creature-xy *game-player*)))
    (unless (not terrain-updated)
      (debug-print 50 "Updating opacity map.~%")
      (set-tcod-opacity-map *game-map* fov-map)
      (setf terrain-updated nil))
    (unless (not fov-updated)
      (debug-print 50 "Updating FOV and lighting.~%")
      (clear-lighting)
      (add-light-from-source *game-torch* fov-map)
      (add-light-from-source *game-brazier* fov-map)
      (tcod:map-compute-fov fov-map
			    (car (creature-xy *game-player*))
			    (cdr (creature-xy *game-player*))
			    (+ map-width map-height)
			    t
			    :fov-shadow)
      (dotimes (x map-width)
	(dotimes (y map-height)
	  (let ((tile (tile-at *game-map* x y)))
	    (debug-print 2000 "Lighting at ~a ~a is ~a.~%" x y (tile-lighting tile))
	    (setf (tile-visible (aref *game-map* x y))
		  (and 
		   (>= (tile-lighting tile) +light-visibility-threshold+)
		   (tcod:map-is-in-fov? fov-map x y))))))
      (setf fov-updated nil))
    
    (tcod:console-set-default-background tcod:*root* tcod-black)
    (tcod:console-clear tcod:*root*)
    (dotimes (x map-width)
      (dotimes (y map-height)
	(let* ((tile (aref *game-map* x y))
	       (appearance (appearance-at tile))
	       (bg-light (coerce (min 1 (tile-lighting tile)) 'float))
	       (fg-light (coerce (min 1 (tile-lighting tile)) 'float))
	       (visible (tile-visible tile)))
	  (unless (null (tile-creature tile))
	    (setf fg-light (coerce (max fg-light +minimum-fg-light+) 'float)))
	  (unless (not (and (= x (player-x))
			    (= y (player-y))))
	    (setf visible t))
	  (unless (not (and (<= (abs (- x (player-x))) 1)
			    (<= (abs (- y (player-y))) 1)
			    (not (tile-walkable tile))))
	    (setf fg-light (coerce (max fg-light +minimum-touched-light+) 'float))
	    (setf bg-light (coerce (max bg-light +minimum-touched-light+) 'float))
	    (setf visible t))
	  (unless (not *cheat-lightall*)
	    (setf visible t
		  bg-light 1.0
		  fg-light 1.0))
	  (unless (not visible)
	    (tcod:console-put-char-ex tcod:*root*
				      x
				      (+ y +ui-top-lines+)
				      (appearance-glyph appearance)
				      (tcod:color-multiply-scalar
				       (apply #'tcod:compose-colour
					      (appearance-foreground-colour appearance))
				       fg-light)
				      (tcod:color-multiply-scalar
				       (apply #'tcod:compose-colour
					      (appearance-background-colour appearance))
				       bg-light))))))
    
    (let ((y-offset 0))
      (tcod:console-set-default-background tcod:*root*
					   (tcod:compose-colour 0 0 0))
      (tcod:console-set-default-foreground tcod:*root*
					   (tcod:compose-colour 0 255 255))
      (dolist (entry (reverse *game-text-buffer*))
	(incf y-offset (tcod:console-print-rect-ex tcod:*root*
						   0
						   y-offset
						   +screen-width+
						   (- +ui-top-lines+ y-offset)
						   :set
						   :left
						   entry))))
    (tcod:console-set-default-background tcod:*root*
					 (tcod:compose-colour 0 0 0))
    (tcod:console-set-default-foreground tcod:*root*
					 (tcod:compose-colour 255 255 255))
    (tcod:console-print-ex tcod:*root*
			   0
			   (- +screen-height+ +ui-bottom-lines+)
			   :set
			   :left
			   (make-statusline))
    (setf *game-text-buffer* nil)
    
    (tcod:console-flush)
    
    (let* ((key (tcod:console-wait-for-keypress t)))
      (handle-keyboard-input key)))
  (terminate))

(defun make-statusline ()
  (format nil "~a [~a/~a]"
	  (creature-name *game-player*)
	  (creature-hp *game-player*)
	  (creature-max-hp *game-player*)))
	  

(defun quit-game ()
    (setf *game-running* nil))

(defun ai-random-walk (creature)
  (let ((moves (remove-if-not #'(lambda (dxdy)
				  (creature-can-walk? creature 
						      (tile-at *game-map*
							       (+ (creature-x creature) (car dxdy))
							       (+ (creature-y creature) (cdr dxdy)))))
			      *directions*)))
    (unless (null moves)
      (let ((xy (select-random moves)))
	(try-move-creature *game-map* creature (car xy) (cdr xy))))))

(defun handle-input (thing)
  (let ((result (assoc thing *game-input-hooks* :test #'equal)))
    (if result
	(funcall (cdr result))
	(debug-print 100 "Unhooked input: ~a.~%" thing))))

(defun handle-keyboard-input (key)
  (debug-print 200 "Handling input: ~a.~%" key)
  (let ((vk (tcod:key-vk key)))
    (cond
      ((eq vk :CHAR) (handle-input (tcod:key-c key)))
      (t (handle-input (tcod:key-vk key))))))

(defun add-input-hook (thing hook)
  (push (cons thing hook) *game-input-hooks*))

(defun tick-world ()
  (tick-creatures *game-creatures*)
  (debug-print 50 "Ticking.~%"))

(defun player-took-action ()
  (tick-world))

(defun player-x () (car (creature-xy *game-player*)))
(defun player-y () (cdr (creature-xy *game-player*)))

(defun try-move-player (dx dy)
  (let ((target (tile-at *game-map* (+ (player-x) dx) (+ (player-y) dy))))
    (cond
      ((or (null target) (not (tile-walkable target)))
       (buffer-show "The way is blocked."))
      ((not (null (tile-creature target)))
       (buffer-show "There's something in the way, and ~a is a pacifist." (creature-name *game-player*)))
      (t 
       (unless (not (try-move-creature *game-map* *game-player* dx dy))
	 (move-light-source *game-torch* (player-x) (player-y))
	 (player-took-action))))))

(defun move-command (dx dy)
  ;; Later, we might not be moving the player, but, say, a cursor.
  (try-move-player dx dy))

(defun reset-standard-input-hooks ()
  (setf *game-input-hooks* nil)
  (add-input-hook #\h #'(lambda () (move-command -1 0)))
  (add-input-hook #\l #'(lambda () (move-command 1 0)))
  (add-input-hook #\j #'(lambda () (move-command 0 1)))
  (add-input-hook #\k #'(lambda () (move-command 0 -1)))
  (add-input-hook #\y #'(lambda () (move-command -1 -1)))
  (add-input-hook #\u #'(lambda () (move-command 1 -1)))
  (add-input-hook #\b #'(lambda () (move-command -1 1)))
  (add-input-hook #\n #'(lambda () (move-command 1 1)))
  (add-input-hook #\V #'(lambda () (setf *cheat-lightall* (not *cheat-lightall*))))
  (add-input-hook #\Q #'quit-game))

(defun initialize-first-game ()
  (let ((map-width +screen-width+)
	(map-height (- +screen-height+ +ui-top-lines+ +ui-bottom-lines+)))
    (setf *game-map* (create-game-map-test map-width map-height))
    (setf *game-player* (spawn-creature 
			 (make-creature
			  :appearance (make-appearance :glyph +player-glyph+
						       :foreground-colour '(0 0 255))
			  :name "Frederick"
			  :hp 20
			  :max-hp 20)
			 *game-map*))
    (setf *game-torch* (make-light-source
			:x (player-x)
			:y (player-y)
			:intensity 0)) ;; Player should not generally carry a torch, but handy for debugging
    (setf *game-brazier* (make-light-source
			  :x 2
			  :y 8
			  :intensity 0.9))
    (spawn-creature (make-creature
		     :appearance (make-appearance :glyph (char-code #\~)
						  :foreground-colour '(0 0 0))
		     :name "Monster"
		     :hp 10
		     :max-hp 10
		     :ai #'ai-random-walk)
		    *game-map*)
    (push "Welcome to Light7DRL!" *game-text-buffer*)
    (buffer-show "How pitiful his tale!")
    (buffer-show "How rare his beauty!")))

(defun buffer-show (&rest rest)
  (push (apply #'format (cons nil rest)) *game-text-buffer*))

(defun start-game ()
  (let* ((game-title "Light7DRL"))
    (load-libraries)
    (unless *game-initialized*
      (initialize-first-game)
      (setf *game-initialized* t))
    (tcod:console-set-custom-font *tileset-file*
				  '(:font-layout-ascii-in-row)
				  16 16)
    (tcod:console-init-root +screen-width+
			    +screen-height+
			    game-title
			    nil ;; not a fullscreen game
			    :renderer-sdl)
    (tcod:console-set-keyboard-repeat 500 10)
    (reset-standard-input-hooks)
    (debug-print 100 "Entering loop.~%")
    (game-loop)
    (debug-print 100 "End loop.~%")))
  

(defun game-main ()
  (start-game)
  (close-libraries)
  (terminate))
