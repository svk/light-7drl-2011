(in-package #:light-7drl)

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

(defstruct light-source
  x
  y
  intensity
  (cached-fov nil))

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


