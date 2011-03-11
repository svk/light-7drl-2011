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
  (xy nil)
  (items nil)
  (level nil))

(defstruct item
  appearance
  name
  size)

(defstruct level
  width
  height
  tiles
  (creatures nil))

(defstruct light-source
  x
  y
  intensity
  (cached-fov nil))

(defun apply-to-all-tiles (map f)
  (dotimes (x (level-width map))
    (dotimes (y (level-height map))
      (funcall f (aref (level-tiles map) x y)))))

(defun apply-to-all-tiles-xy (map f)
  (dotimes (x (level-width map))
    (dotimes (y (level-height map))
      (funcall f (aref (level-tiles map) x y) x y))))

(defun apply-to-all-xy (f)
  (dotimes (x (level-width *game-current-level*))
    (dotimes (y (level-height *game-current-level*))
      (funcall f x y))))

(defun clear-tile-light (tile)
  (setf (tile-lighting tile) 0))

(defun clear-lighting (&optional (level *game-current-level*))
  (apply-to-all-tiles level #'clear-tile-light))

(defun distance (ax ay bx by)
  (let ((dx (- ax bx))
	(dy (- ay by)))
    (sqrt (+ (* dx dx) (* dy dy)))))

(defun extract-fov (fov-map)
  (let ((rv (make-array (array-dimensions (level-tiles *game-current-level*)))))
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
     *game-current-level*
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

(defun tile-at (level x y)
  (let ((map (level-tiles level)))
    (unless (not (array-in-bounds-p map x y))
      (aref map x y))))

(defun try-move-creature (creature dx dy)
  (let* ((level (creature-level creature))
	 (xy (creature-xy creature))
	 (x (car xy))
	 (y (cdr xy))
	 (xp (+ x dx))
	 (yp (+ y dy))
	 (tile (tile-at level xp yp))
	 (map (level-tiles level)))
    (unless (not (creature-can-walk? creature tile))
      (set-creature-position creature level xp yp)
      tile)))

(defun set-creature-position (creature level x y)
  (unless (null (creature-tile creature))
    (setf (tile-creature (creature-tile creature)) nil))
  (setf (tile-creature (tile-at level x y)) creature
	(creature-level creature) level
	(creature-xy creature) (cons x y)
	(creature-tile creature) (tile-at level x y)))

(defun spawn-creature (creature level)
  (let* ((xy (select-random 
	     (remove-if-not #'(lambda (xy) (creature-can-walk? creature (tile-at level (car xy) (cdr xy))))
			    (find-walkables level))))
	 (x (car xy))
	 (y (cdr xy)))
    (debug-print 50 "Spawning creature ~a on ~a.~%" creature xy)
    (set-creature-position creature level x y)
    (push creature (level-creatures level))
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

(defun tile-dark (tile)
  (< (tile-lighting tile) +light-visibility-threshold+))

(defun appearance-at (tile)
  (unless (not (tile-creature tile))
    (return-from appearance-at
      (overlay-appearances
       (creature-appearance (tile-creature tile))
       (tile-appearance tile))))
  (unless (not (tile-items tile))
    (return-from appearance-at
      (overlay-appearances
       (item-appearance (car (tile-items tile)))
       (tile-appearance tile))))
  (tile-appearance tile))

(defun create-level (width height)
  (let ((rv (make-level :width width
	      :height height
	      :tiles (make-array (list width height)
				 :initial-element (make-floor-tile))
	      :creatures nil)))
    (dotimes (x width)
      (dotimes (y height)
	(setf (aref (level-tiles rv) x y)
	      (if (or (= x 0)
		      (= y 0)
		      (= (+ 1 x) width)
		      (= (+ 1 y) height))
		  (make-floor-tile)
		  (make-floor-tile)))))
    rv))

(defun create-level-test (width height)
  (let ((level (create-level width height)))
    (dotimes (x width)
      (dotimes (y height)
	(setf (aref (level-tiles level) x y) (if (and (= 0 (random 8))
					   (< 10 (distance 2 8 x y)))
				      (make-wall-tile)
				      (make-floor-tile)))))
    level))

(defun find-walkables (level)
  (do* ((width (level-width level))
	(height (level-height level))
	(x 0 (+ x 1))
	(rv nil))
       ((>= x width) rv)
    (dotimes (y height)
      (unless (not (tile-walkable (aref (level-tiles level) x y)))
	(push (cons x y) rv)))))

(defun select-random (list)
  (nth (random (length list)) list))

(defun find-random-walkable (map)
  (select-random (find-walkables map)))

(defun ai-random-walk (creature)
  (debug-print 50 "AI random-walk triggers on ~a." (creature-name creature))
  (let ((moves (remove-if-not #'(lambda (dxdy)
				  (creature-can-walk? creature 
						      (tile-at (creature-level creature)
							       (+ (creature-x creature) (car dxdy))
							       (+ (creature-y creature) (cdr dxdy)))))
			      *directions*)))
    (unless (null moves)
      (let ((xy (select-random moves)))
	(try-move-creature creature (car xy) (cdr xy))))))

(defun tick-world ()
  (tick-creatures (level-creatures *game-current-level*))
  (debug-print 50 "Ticking.~%"))

(defun player-took-action ()
  (setf *buffer-clear-time* t)
  (tick-world))

(defun player-x () (car (creature-xy *game-player*)))
(defun player-y () (cdr (creature-xy *game-player*)))

(defun creature-give (creature item)
  (debug-print 10 "Giving item \"~a\" to creature \"~a\"" (item-name item) (creature-name creature))
  (push item (creature-items creature))
  (debug-print 10 "Inventory is now ~a.~%" (mapcar #'item-name (creature-items creature)))
  item)

(defun creature-take (creature item)
  (debug-print 10 "Taking item \"~a\" from creature \"~a\"" (item-name item) (creature-name creature))
  (setf (creature-items creature) (remove item (creature-items creature)))
  item)

(defun creature-pick-up (creature item)
  (let ((tile (creature-tile creature)))
    (setf (tile-items tile) (remove item (tile-items tile)))
    (creature-give creature item)))

(defun creature-drop (creature item)
  (unless (null (creature-take creature item))
    (push item (tile-items (creature-tile creature)))
    item))

(defun try-player-pick-up (item)
  (unless (null (creature-pick-up *game-player* item))
    (buffer-show "You pick up ~a." (definite-noun (item-name item)))))

(defun try-player-drop (item &optional (confirmed nil))
  (cond
    ((and (not confirmed)
	  (tile-dark (creature-tile *game-player*)))
     (query-confirm (format nil
			    "Really drop ~a in complete darkness? It might be hard to retrieve it."
			    (definite-noun (item-name item)))
		    #'(lambda () (try-player-drop item t))))
    (t (unless (null (creature-drop *game-player* item))
	 (buffer-show "You drop ~a." (definite-noun (item-name item)))))))

(defun try-player-drop-stack ()
  (if (null (creature-items *game-player*))
      (buffer-show "You're not holding anything you could drop.")
      (try-player-drop (car (creature-items *game-player*)))))

(defun try-player-pick-up-stack ()
  (let ((tile (tile-at *game-current-level* (player-x) (player-y))))
    (cond
      ((not (tile-visible (creature-tile *game-player*)))
       (buffer-show "It's too dark to make out any items on the floor here."))
      ((null (tile-items tile))
       (buffer-show "There's nothing here to pick up."))
      (t (try-player-pick-up (car (tile-items tile)))))))

(defun try-move-player (dx dy)
  (let ((target (tile-at *game-current-level* (+ (player-x) dx) (+ (player-y) dy))))
    (cond
      ((or (null target) (not (tile-walkable target)))
       (buffer-show "The way is blocked."))
      ((not (null (tile-creature target)))
       (buffer-show "There's something in the way, and ~a is a pacifist." (creature-name *game-player*)))
      (t 
       (let ((old-tile (creature-tile *game-player*))
	     (new-tile (try-move-creature *game-player* dx dy)))
	 (unless (not new-tile)
	   (move-light-source *game-torch* (player-x) (player-y))
	   (cond ((and (tile-dark old-tile)
		       (not (tile-dark new-tile)))
		  (buffer-show "Your eyes blink as you adjust to the light."))
		 ((and (not (tile-dark old-tile))
		       (tile-dark new-tile))
		  (buffer-show "You stumble into the darkness.")))
	   (unless (null (tile-items new-tile))
	     (buffer-show (summarize-floor-items (tile-items new-tile))))
	   (player-took-action)))))))

(defun summarize-floor-items (items)
  (let* ((countlist (make-count-list (mapcar #'item-name items)))
	 (total (reduce #'+ (mapcar #'cdr countlist)))
	 (stringlist (mapcar #'(lambda (noun-count)
				(indefinite-counted-noun (car noun-count)
							 (cdr noun-count)))
			     countlist)))
    (if (null stringlist)
	(format nil "There's nothing here.")
	(format nil "There ~a ~a here." (is-are total) (list-join stringlist)))))
  
(defun initialize-first-game ()
  (query-string
   "Enter your name:"
   #'(lambda (player-name)
       (initialize-first-game-with-info player-name))))

(defun initialize-first-game-with-info (player-name)
  (let ((map-width +screen-width+)
	(map-height (- +screen-height+ +ui-top-lines+ +ui-bottom-lines+)))
    (setf *game-current-level* (create-level-test map-width map-height))
    (setf *game-player* (spawn-creature 
			 (make-creature
			  :appearance (make-appearance :glyph +player-glyph+
						       :foreground-colour '(0 0 255))
			  :name player-name
			  :hp 20
			  :max-hp 20)
			 *game-current-level*))
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
		    *game-current-level*)
    (creature-give *game-player*
		   (make-item :appearance (make-appearance :glyph +weapon-glyph+
							   :foreground-colour '(0 0 0))
			      :name n-sword
			      :size 1))
    (debug-print 50 "Printing welcome messages.~%")
    (buffer-show "Welcome to Light7DRL!")
    (buffer-show "How pitiful his tale!")
    (buffer-show "How rare his beauty!")
    (debug-print 100 "Buffer is now: ~a.~%" *game-text-buffer*)))

(defun cheat-spawn-thingy ()
  (creature-give *game-player*
		 (make-item :appearance (make-appearance :glyph +weapon-glyph+
							 :foreground-colour '(0 0 0))
			    :name n-goose
			    :size 1)))

(defun cheat-spawn-doodad ()
  (creature-give *game-player*
		 (make-item :appearance (make-appearance :glyph +weapon-glyph+
							 :foreground-colour '(0 0 0))
			    :name n-child
			    :size 1)))



