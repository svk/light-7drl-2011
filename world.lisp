(in-package #:light-7drl)

(defun level-set-obstacle-map (level omap)
  (with-slots (obstacle-map)
      level
    (setf obstacle-map omap)))

(defmethod print-object ((level level) stream)
  (format stream "[Level]"))

(defmethod print-object ((ls light-source) stream)
  (format stream "[light source at ~a ~a]" (light-source-x ls) (light-source-y ls)))

(defmethod print-object ((creature creature) stream)
  (format stream "[Creature: ~a]" (definite-noun (creature-name creature))))

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

(defun level-acquire-obstacle-map (level)
  (with-slots (obstacle-map obstacle-map-updated tiles)
      level
    (let ((rv obstacle-map))
      (unless (null rv)
	(debug-print 50 "Acquired obstacle map..~%"))
      (unless (or (null obstacle-map)
		  obstacle-map-updated)
	(set-tcod-opacity-map tiles obstacle-map))
      (setf obstacle-map nil)
      rv)))

(defun level-release-obstacle-map (level omap)
  (unless (null omap)
      (debug-print 50 "Released obstacle map..~%")
    (level-set-obstacle-map level omap)))

(defun xy-in-bounds? (xy)
  (and (>= (car xy) 0)
       (>= (cdr xy) 0)
       (< (car xy) (level-width *game-current-level*))
       (< (cdr xy) (level-height *game-current-level*))))

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

(defun extract-fov-coordinates (fov-map)
  (let ((rv nil))
    (apply-to-all-xy #'(lambda (x y)
			 (unless (not (tcod:map-is-in-fov? fov-map x y))
			   (push (list x y) rv))))
    rv))

(defun extract-fov-xys (fov-map)
  (let ((rv nil))
    (apply-to-all-xy #'(lambda (x y)
			 (unless (not (tcod:map-is-in-fov? fov-map x y))
			   (push (cons x y) rv))))
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
  (let* ((level (object-level creature))
	 (xy (creature-xy creature))
	 (x (car xy))
	 (y (cdr xy))
	 (xp (+ x dx))
	 (yp (+ y dy))
	 (tile (tile-at level xp yp))
	 (map (level-tiles level)))
    (unless (not (creature-can-walk? creature tile))
      (set-position creature xp yp level)
      tile)))

(defun spawn-creature (creature level)
  (let* ((xy (select-random 
	     (remove-if-not #'(lambda (xy) (creature-can-walk? creature (tile-at level (car xy) (cdr xy))))
			    (find-walkables level))))
	 (x (car xy))
	 (y (cdr xy)))
    (debug-print 50 "Spawning creature ~a on ~a.~%" creature xy)
    (set-position creature x y level)
    (debug-print 50 "Newly spawned creature on ~a.~%" (object-level creature))
    creature))

(defun tick-creatures (list)
  (dolist (creature list)
    (tick creature)))

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
	      :creatures nil
	      :obstacle-map (tcod:map-new width height)
	      :obstacle-map-updated nil)))
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

(defun postprocess-level (level)
  (level-set-all-unexplored level)
  (level-add-delayed-spawn level
			   50
			   #'(lambda (choices) (mutate-random-tile
						level
						choices
						#'place-lever
						:eligible #'not-special?)))
  level)

(defun create-level-generated (width height)
  (let ((level (create-level width height))
	(sketch (generate-cave (- width 2) (- height 2))))
    (format t "~a" sketch)
    (dotimes (x width)
      (dotimes (y height)
	(setf (aref (level-tiles level) x y)
	      (cond
		((not (array-in-bounds-p sketch (- x 1) (- y 1)))
		 (make-wall-tile))
		((eql +sketch-floor+ (aref sketch (- x 1) (- y 1))) (make-floor-tile))
		(t (make-wall-tile))))))
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

(defun find-random-walkable (map)
  (select-random (find-walkables map)))

(defun fov->fov-xys (fov)
  (let ((rv nil))
    (dotimes (x +map-width+)
      (dotimes (y +map-height+)
	(unless (not (aref fov x y))
	  (push (cons x y) rv))))
    rv))

(defun update-world ()
  (clear-lighting)
  (dolist (item (level-floor-items *game-current-level*))
    (emit-light item))
  (dolist (creature (level-creatures *game-current-level*))
    (emit-light creature))
  (unless (not *game-player*)
    (let* ((newly-explored (level-explore *game-current-level*
					  (fov->fov-xys (get-fov *game-player*))))
	   (left-unexplored (length (level-unexplored *game-current-level*))))
      (debug-print 50
		   "Newly explored tiles: ~a Unexplored tiles: ~a dspawns ~a~%"
		   (length newly-explored)
		   left-unexplored
		   (level-delayed-spawns *game-current-level*))
      (dolist (delayed-spawn (level-delayed-spawns *game-current-level*))
	(unless (<= (car delayed-spawn) left-unexplored)
	  (or (funcall (cadr delayed-spawn) (append newly-explored
						    (level-unexplored *game-current-level*)))
	      (funcall (cadr delayed-spawn) (find-walkables *game-current-level*)))
	  (setf (level-delayed-spawns *game-current-level*)
		(remove delayed-spawn
			(level-delayed-spawns *game-current-level*)))))))
  (let ((fov-map (level-acquire-obstacle-map *game-current-level*)))
    (debug-print 10 "omap comin': ~a~%" fov-map)
    (add-light-from-source *game-torch* fov-map)
    (level-release-obstacle-map *game-current-level* fov-map)))
  

(defun tick-world ()
  ;; Notice a that could theoretically come back to bite me, but hopefully will
  ;; not within the next 24 hours: lighting, etc., is updated only once per
  ;; turn, while light sources might move several times during a turn. Lighting
  ;; might be in an inconsistent state during a turn (inconsistent with the
  ;; light sources).
  (tick-creatures (level-creatures *game-current-level*))
  (debug-print 50 "Ticking.~%")
  (update-world))

(defun player-wait ()
  (player-taking-action)
  (player-took-action))

(defun player-taking-action ()
  (buffer-clear))

(defun player-took-action ()
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
    (picked-up item)
    (creature-give creature item)))

(defun creature-drop (creature item)
  (unless (null (creature-take creature item))
    (drop-at item (object-level creature) (creature-x creature) (creature-y creature))
    item))

(defun select-n-random (n list)
  (if (<= (length list) n)
      list
      (do ((rv-indices nil))
	  ((>= (length rv-indices) n)
	   (mapcar #'(lambda (i) (nth i list)) rv-indices))
	(pushnew (random (length list)) rv-indices))))

(defun generate-light-source-cover (level intensity-gen &optional (target 1) (estimate nil))
  ;; Greedy algo, v likely nonoptimal but should be fine for common cases
  ;; (Even the one that's not an "estimate" is still not guaranteed optimal!
  (let* ((all-xys (find-walkables level))
	 (dark-xys (copy-list all-xys))
	 (lights nil)
	 (obstacle-map (level-acquire-obstacle-map level))
	 (factor *universal-light-half-life*)
	 (threshold (/ 1 255))
	 (acceptable-dark (truncate (* (- 1 target) (length dark-xys)))))
    (do ((intensity (funcall intensity-gen) (funcall intensity-gen)))
	((<= (length dark-xys) acceptable-dark)  lights)
      (let ((best-x nil)
	    (best-y nil)
	    (best-new-lit-no 0)
	    (best-new-lit nil))
	(debug-print 50 "Beginning light coverage round.~%")
	(dolist (xy (if (not estimate)
			all-xys
			(select-n-random 200 dark-xys)))
	  (let* ((x (car xy))
		 (y (cdr xy))
		 (fov (extract-fov-xys
		       (progn
			 (tcod:map-compute-fov obstacle-map
					       x
					       y
					       (max (level-width level) (level-height level))
					       t
					       :fov-shadow)
			 obstacle-map)))
		 (potential-lit (intersection dark-xys fov :test #'equal))
		 (actual-lit
		  (remove-if-not
		   #'(lambda (xy)
			     (let ((extra-light
				    (* intensity (exp (* factor (distance x y (car xy) (cdr xy)))))))
			       (>= (+ (tile-lighting (tile-at level (car xy) (cdr xy)))
				      extra-light)
				   threshold)))
		   potential-lit))
		 (actual-lit-no (length actual-lit)))
	    (unless (<= actual-lit-no best-new-lit-no)
	      (setf best-new-lit-no actual-lit-no
		    best-new-lit actual-lit
		    best-x x
		    best-y y))))
	(debug-print 50 "Light coverage round over: winner ~a ~a, ~a new lit tiles.~%" best-x best-y best-new-lit-no)
	(setf dark-xys (set-difference dark-xys best-new-lit :test #'equal))
	(push (make-light-source :x best-x
				 :y best-y
				 :intensity intensity)
	      lights)
	(add-light-from-source (car lights) obstacle-map)))
    (level-release-obstacle-map level obstacle-map)
    (clear-lighting level)
    lights))

(defun try-player-pick-up (item)
  (unless (null (creature-pick-up *game-player* item))
    (player-took-action)
    (buffer-show "You pick up ~a." (definite-noun (item-name item)))))

(defun try-player-drop (item &optional (confirmed nil))
  (player-taking-action)
  (cond
    ((and (not confirmed)
	  (tile-dark (creature-tile *game-player*)))
     (query-confirm (format nil
			    "Really drop ~a in complete darkness? It might be hard to retrieve it."
			    (definite-noun (item-name item)))
		    #'(lambda () (try-player-drop item t))))
    (t (unless (null (creature-drop *game-player* item))
	 (player-took-action)
	 (buffer-show "You drop your ~a." (noun-singular (item-name item)))))))

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
  (player-taking-action)
  (let ((target (tile-at *game-current-level* (+ (player-x) dx) (+ (player-y) dy))))
    (cond
      ((or (null target) (not (tile-walkable target)))
       (buffer-show "The way is blocked."))
      ((not (null (tile-creature target)))
       (let* ((creature (tile-creature target)))
	 (melee-attack creature *game-player*)
	 (player-took-action)))
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
	   (let ((summary (summarize-tile new-tile)))
	     (unless (zerop (length summary))
	       (buffer-show summary)))
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

(defun summarize-tile (tile)
  (let* ((items (tile-items tile))
	 (countlist (make-count-list (mapcar #'item-name items)))
	 (total (reduce #'+ (mapcar #'cdr countlist))))
    (if (tile-special tile)
	(progn
	  (incf total)
	  (push (case (tile-special tile)
		  (:lever (cons n-lever 1))
		  (:hole (cons n-hole 1)))
		countlist)))
    (let ((stringlist (mapcar #'(lambda (noun-count)
				  (indefinite-counted-noun (car noun-count)
							   (cdr noun-count)))
			      countlist)))
      (if (null stringlist)
	  (format nil "There's nothing here.")
	  (capitalize (format nil
			      "There ~a ~a here." 
			      (is-are (cdar countlist))
			      (list-join stringlist)))))))
  
(defun initialize-first-game ()
  (query-string
   "Enter your name: "
   #'(lambda (player-name)
       (query-letterset
	"Are you [m]ale or [f]emale? "
	'(#\m #\f)
	(lambda (player-genderletter)
	  (initialize-first-game-with-info player-name
					   (case player-genderletter
					     (#\m :male)
					     (#\f :female)
					     (t nil))))))))

(defun uninitialize-game ()
  (setf *game-special-screen-stack* nil
	*game-text-buffer* nil
	*game-current-level* nil
	*game-player* nil
	*game-initialized* nil))

(defun initialize-first-game-with-info (player-name player-gender)
  (let ((map-width +screen-width+)
	(map-height (- +screen-height+ +ui-top-lines+ +ui-bottom-lines+)))
    (debug-print 50 "INITIALIZING GAME HELLO")
    (push-hooks #'ignore-input)
    (push-special-screen (make-centered-text-special-screen "Please wait..."))
    (setf *game-current-level*
	  (postprocess-level (create-level-generated map-width map-height)))
    (setf *game-player* (spawn-creature 
			 (make-creature
			  :appearance (make-appearance :glyph +player-glyph+
						       :foreground-colour '(0 0 255))
			  :name (make-proper-noun player-name)
			  :gender player-gender
			  :hit-chance (make-chance-roll :success-chance 3/4)
			  :damage (make-dice-roll :number-of-dice 2
						  :dice-size 6
						  :constant 4)
			  :max-hp 100)
			 *game-current-level*))
    (install-hook *game-player* :before-death
		  #'(lambda ()
		      (debug-print 1 "Triggering player death hook.~%")
		      (signal 'game-over
			      :type :death)))
    (debug-print 50 "Game-player is now: ~a.~%" *game-player*)
;    (let ((light-sources (generate-light-source-cover *game-current-level* (const 0.5) 1 t)))
;      (dolist (ls light-sources)
;	(push ls *game-braziers*))
;      (debug-print 50 "Generated light sources: ~a.~%" light-sources))
    (setf *game-torch* (make-light-source
			:x (player-x)
			:y (player-y)
			:intensity 0)) ;; Player should not generally carry a torch, but handy for debugging
    (debug-print 50 "what the fuck should spawn creature")
    (dotimes (i 5)
	(spawn-creature (make-snake) *game-current-level*))
    (debug-print 50 "what the fuck should have spawned creature")
    (creature-give *game-player*
		   (make-item :appearance (make-appearance :glyph +weapon-glyph+
							   :foreground-colour '(0 0 0))
			      :name n-sword
			      :light-source (make-light-source :x nil
							       :y nil
							       :intensity 0.9)))
    (debug-print 50 "Printing welcome messages.~%")
    (buffer-show "Welcome to Light7DRL!")
    (buffer-show "How pitiful ~a tale!" (player-possessive))
    (buffer-show "How rare ~a beauty!" (player-possessive))
    (debug-print 100 "Buffer is now: ~a.~%" *game-text-buffer*)
    (update-world)
    (pop-hooks)
    (pop-special-screen)))

(defun player-possessive ()
  (third-person-possessive (creature-gender *game-player*)))

(defun player-singular ()
  (third-person-singular (creature-gender *game-player*)))

(defun cheat-spawn-thingy ()
  (creature-give *game-player*
		 (make-item :appearance (make-appearance :glyph +weapon-glyph+
							 :foreground-colour '(0 0 0))
			    :name n-goose)))

(defun cheat-spawn-doodad ()
  (creature-give *game-player*
		 (make-item :appearance (make-appearance :glyph +weapon-glyph+
							 :foreground-colour '(0 0 0))
			    :name n-child)))

(defmethod emit-light ((radiant radiant))
  (with-slots (light-source level)
      radiant
    (unless (or (null light-source)
		(null level))
      (debug-print 50 "Radiant ~a is emitting light.~%" radiant)
      (let ((fov-map (level-acquire-obstacle-map level)))
	(add-light-from-source light-source fov-map)
	(level-release-obstacle-map level fov-map)))))




