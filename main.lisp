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
(defconstant +screen-height+ 30)

(defparameter *tileset-file* "my-tiles.png")

(defconstant +ui-top-lines+ 3)
(defconstant +ui-bottom-lines+ 2)

(defparameter *game-map* nil)
(defparameter *game-player* nil)
(defparameter *game-running* nil)
(defparameter *game-text-buffer* nil)

(defparameter *game-initialized* nil)

(defparameter *debug-level* 100)

(defparameter *game-input-hooks* nil)

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
  (items nil))

(defstruct creature
  appearance
  name
  (tile nil)
  (xy nil))

(defun creature-can-walk? (creature tile)
  (declare (ignore creature))
  (and
   tile
   (tile-walkable tile)
   (not (tile-creature tile))))

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
    creature))

(defun make-wall-tile ()
  (make-tile :appearance (make-appearance :glyph +wall-glyph+
					  :foreground-colour '(0 0 255)
					  :background-colour '(0 0 0))
	     :opaque t
	     :walkable nil))

(defun make-floor-tile ()
  (make-tile :appearance (make-appearance :glyph +floor-glyph+
					  :foreground-colour '(255 255 255)
					  :background-colour '(255 255 255))
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
		  (make-wall-tile)
		  (make-floor-tile)))))
    rv))

(defun create-game-map-test (width height)
  (let ((map (create-game-map width height)))
    (setf (aref map 10 10) (make-wall-tile))
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
      (debug-print 50 "Updating FOV.~%")
      (tcod:map-compute-fov fov-map
			    (car (creature-xy *game-player*))
			    (cdr (creature-xy *game-player*))
			    (+ map-width map-height)
			    t
			    :fov-shadow)
      (dotimes (x map-width)
	(dotimes (y map-height)
	  (setf (tile-visible (aref *game-map* x y))
		(tcod:map-is-in-fov? fov-map x y))))
      (setf fov-updated nil))
    
    (tcod:console-set-default-background tcod:*root* tcod-black)
    (tcod:console-clear tcod:*root*)
    (dotimes (x map-width)
      (dotimes (y map-height)
	(let* ((tile (aref *game-map* x y))
	       (appearance (appearance-at tile)))
	  (unless (not (tile-visible tile))
	    (tcod:console-put-char-ex tcod:*root*
				      x
				      (+ y +ui-top-lines+)
				      (appearance-glyph appearance)
				      (apply #'tcod:compose-colour
					     (appearance-foreground-colour appearance))
				      (apply #'tcod:compose-colour
					     (appearance-background-colour appearance)))))))
    (tcod:console-flush)
    
    (let* ((key (tcod:console-wait-for-keypress t)))
      (handle-keyboard-input key)))
  (terminate))

(defun quit-game ()
    (setf *game-running* nil))

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
  (debug-print 50 "Ticking.~%"))

(defun player-took-action ()
  (tick-world))

(defun try-move-player (dx dy)
  (unless (not (try-move-creature *game-map* *game-player* dx dy))
    (player-took-action)))

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
  (add-input-hook #\Q #'quit-game))

(defun initialize-first-game ()
  (let ((map-width +screen-width+)
	(map-height (- +screen-height+ +ui-top-lines+ +ui-bottom-lines+)))
    (setf *game-map* (create-game-map-test map-width map-height))
    (setf *game-player* (spawn-creature 
			 (make-creature
			  :appearance (make-appearance :glyph +player-glyph+
						       :foreground-colour '(255 0 0))
			  :name "Frederick")
			 *game-map*))
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
    (tcod:console-set-keyboard-repeat 500 100)
    (reset-standard-input-hooks)
    (debug-print 100 "Entering loop.~%")
    (game-loop)
    (debug-print 100 "End loop.~%")))
  

(defun game-main ()
  (start-game)
  (close-libraries)
  (terminate))
