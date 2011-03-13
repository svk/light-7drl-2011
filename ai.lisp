(in-package :light-7drl)

(defun ai-random-walk-satisfying (creature f)
  (let ((moves (remove-if-not #'(lambda (dxdy)
				  (let ((tile (tile-at (object-level creature)
						       (+ (creature-x creature) (car dxdy))
						       (+ (creature-y creature) (cdr dxdy)))))
				    (and 
				     (creature-can-walk? creature tile)
				     (funcall f tile))))
			      *directions*)))
    (unless (null moves)
      (let ((xy (select-random moves)))
	(try-move-creature creature (car xy) (cdr xy))))))

(defun ai-random-dark-walk (creature)
  (ai-random-walk-satisfying creature #'tile-dark))
  

(defun ai-random-walk (creature)
  (debug-print 50 "AI random-walk triggers on ~a." (creature-name creature))
  (let ((moves (remove-if-not #'(lambda (dxdy)
				  (creature-can-walk? creature 
						      (tile-at (object-level creature)
							       (+ (creature-x creature) (car dxdy))
							       (+ (creature-y creature) (cdr dxdy)))))
			      *directions*)))
    (unless (null moves)
      (let ((xy (select-random moves)))
	(try-move-creature creature (car xy) (cdr xy))))))

(defun ai-random-walk-restricted (creature restrict)
  (let ((moves (remove-if-not #'(lambda (dxdy)
				  (creature-can-walk? creature 
						      (tile-at (object-level creature)
							       (+ (creature-x creature) (car dxdy))
							       (+ (creature-y creature) (cdr dxdy)))))
			      *directions*)))
    (unless (null moves)
      (let ((xy (select-random (funcall restrict moves))))
	(try-move-creature creature (car xy) (cdr xy))))))


(defun make-step-or-random (creature step)
  (if (or (null step)
	  (not (try-move-creature creature (car step) (cdr step))))
      (ai-random-walk creature)))

(defun naive-step-to (target creature)
  (let* ((x1y1 (creature-xy target))
	 (x0y0 (creature-xy creature))
	 (dx (- (car x1y1) (car x0y0)))
	 (dy (- (cdr x1y1) (cdr x0y0))))
    (cons (sign dx) (sign dy))))

(defun select-random-or-direct-to (target creature)
  (let ((step (naive-step-to target creature)))
    #'(lambda (steps)
	(debug-print 50 "would prefer ~a, choices ~a~%" step steps)
	(if (find step steps :test #'equal)
	    step
	    (select-random steps)))))

(defun seminaive-step-to (target creature stepmap selector)
  (or (follow-stepmap creature stepmap selector)
      (naive-step-to target creature)))

(defun stepmap-lookup (stepmap xy)
  (if (array-in-bounds-p stepmap (car xy) (cdr xy))
      (aref stepmap (car xy) (cdr xy))
      nil))

(defun follow-stepmap (me stepmap selector)
  (let ((best-steps nil)
	(best-value (stepmap-lookup stepmap (creature-xy me))))
    (dolist (neighbour-xy (neighbours-of-xy (creature-xy me)))
      (let ((val (stepmap-lookup stepmap neighbour-xy)))
	(if (or (null best-value)
		(and val
		     (<= val best-value)))
	    (progn
	      (if (or (null best-value)
		      (< val best-value))
		  (setf best-steps nil))
	      (push neighbour-xy best-steps)
	      (setf best-value val)))))
    (unless (null best-value)
      (funcall selector (mapcar #'(lambda (xy) (cons (- (car xy) (creature-x me))
						     (- (cdr xy) (creature-y me))))
				best-steps)))))

(defun follow-stepmap-search (me stepmap depth selector)
  (or (follow-stepmap me stepmap selector)
      (let* ((level (object-level me))
	     (search-map (make-array (array-dimensions (level-tiles level))
				     :initial-element nil))
	     (my-x (creature-x me))
	     (my-y (creature-y me))
	     (q-tail (cons nil nil))
	     (q q-tail))
	(flet ((qadd (r)
		 (setf (car q-tail) r)
	     (setf (cdr q-tail) (cons nil nil))
	     (setf q-tail (cdr q-tail))))
	  (dolist (neighbour-xy (remove-if-not #'xy-in-bounds? (neighbours-of my-x my-y)))
	    (setf (aref search-map
			(car neighbour-xy)
			(cdr neighbour-xy))
		  (list (cons (- (car neighbour-xy) my-x)
			      (- (cdr neighbour-xy) my-y))
			1))
	    (qadd neighbour-xy))
	  (setf (aref search-map my-x my-y) (list (cons 0 0) 0))
	  (do* ((qe (pop q) (pop q)))
	       ((null qe) nil)
	    (unless (>= (cadr (aref search-map (car qe) (cdr qe)))
		    depth)
	      (dolist (neighbour-xy (remove-if-not #'xy-in-bounds? (neighbours-of-xy qe)))
					;	    (debug-print 50 "exploring ~a [~a ~a]~%" neighbour-xy
					;			 (aref search-map (car neighbour-xy) (cdr neighbour-xy))
					;			 (not (tile-walkable (tile-at level (car neighbour-xy) (cdr neighbour-xy)))))
		(unless (or 
			 (not (null (aref search-map (car neighbour-xy) (cdr neighbour-xy))))
			 (not (tile-walkable (tile-at level (car neighbour-xy) (cdr neighbour-xy)))))
;	      (debug-print 50 "acceptable ~a~%" neighbour-xy)
		  (setf (aref search-map (car neighbour-xy) (cdr neighbour-xy))
			(list
			 (car (aref search-map (car qe) (cdr qe)))
			 (+ 1 (cadr (aref search-map (car qe) (cdr qe))))))
;		  (debug-print 50 "working at ~a~%" (aref search-map (car neighbour-xy) (cdr neighbour-xy)))
		  (qadd neighbour-xy)
		  (if (stepmap-lookup stepmap neighbour-xy)
		      (progn
;			(debug-print 50
;				 "stepmap-search yielded result: ~a first best step to ~a [dist ~a]~%"
;				 (aref search-map (car qe) (cdr qe))
;				 neighbour-xy
;				 (stepmap-lookup stepmap neighbour-xy))
;			(let ((blah (make-array (array-dimensions search-map))))
;			  (dotimes (x +map-width+)
;			    (dotimes (y +map-height+)
;			      (setf (aref blah x y)
;				    (if (aref search-map x y)
;					(cadr (aref search-map x y))
;					nil))))
;	;		  (debug-print 50 "~%~a~%" blah))
			(return-from follow-stepmap-search
			  (car (aref search-map (car qe) (cdr qe))))))))))))))

(defun sad-search (target creature)
  (or
   (follow-stepmap creature
		   (make-tactical-stepmap-to target creature)
		   (select-random-or-direct-to target creature))
   (follow-stepmap-search creature
			  (get-stepmap-to target)
			  15
			  (select-random-or-direct-to target creature))
   (naive-step-to target creature)
   (select-random *directions*)))

(defun ai-seek-darkness (creature)
  (let ((tac-view (find-reachable-tiles creature 3)))
    (flet ((get-lighting (list) (tile-lighting (third list))))
      (let ((minimal-lighting (apply #'min (mapcar #'get-lighting tac-view))))
	(let ((minimal-lists (remove-if-not
			      #'(lambda (z) (equal z minimal-lighting))
			      tac-view
			      :key #'get-lighting)))
	  (let ((selected-list (select-random minimal-lists)))
	    (debug-print 50 "~a selected list: ~a~%" creature selected-list)
	    (let ((selected-step (second selected-list)))
	      (unless (null selected-step)
		(try-move-creature creature
				   (- (car selected-step)
				      (creature-x creature))
				   (- (cdr selected-step)
				      (creature-y creature)))))))))))

           

(defun xy-is-wall? (xy)
  (let ((level (object-level *game-player*)))
    (or (not (array-in-bounds-p (level-tiles level) (car xy) (cdr xy)))
	(not (tile-walkable (tile-at level (car xy) (cdr xy)))))))

(defun extract-values (array)
  (let ((rv nil))
    (dotimes (x +map-width+)
      (dotimes (y +map-height+)
	(unless (null (aref array x y))
	  (push (aref array x y) rv))))
    rv))

(defun find-reachable-tiles (creature depth &optional (xy-is-obstacle? #'xy-is-wall?))
  (let* ((q-end (cons nil nil))
	 (q q-end)
	 (x (creature-x creature))
	 (y (creature-y creature))
	 (level (object-level creature))
	 (a (make-array (array-dimensions (level-tiles (object-level creature))) :initial-element nil)))
    (flet ((addq (elt)
	     (setf (car q-end) elt)
	     (setf q-end
		   (setf (cdr q-end) (cons nil nil)))))
      (setf (aref a x y) (list 0 nil (tile-at level x y) (cons x y)))
      (dolist (nxy (remove-if xy-is-obstacle? (neighbours-of x y)))
	(setf (aref a (car nxy) (cdr nxy)) (list 1 nxy (tile-at level (car nxy) (cdr nxy)) nxy))
	(addq nxy))
      (do ((qe (car q) (car q))
	   (q (cdr q) (cdr q)))
	  ((null qe) (extract-values a))
	(dolist (nxy (remove-if xy-is-obstacle? (neighbours-of (car qe) (cdr qe))))
	  (let ((old-value (aref a (car qe) (cdr qe)))
		(n-x (car nxy))
		(n-y (cdr nxy)))
	    (unless (aref a n-x n-y)
	      (unless (<= (+ 1 (first old-value)) depth)
		(return-from find-reachable-tiles (extract-values a)))
	      (setf (aref a n-x n-y)
		    (list (+ 1 (first old-value)) (second old-value) (tile-at level n-x n-y) nxy))
	      (addq nxy))))))))
	       

(defun sad-flee (origin creature)
  (let ((step (sad-search origin creature)))
    (unless (null step)
      (cons (- (car step))
	    (- (cdr step))))))

(defun naive-flee (origin creature)
  (let ((step (naive-step-to origin creature)))
    (unless (null step)
      (cons (- (car step))
	    (- (cdr step))))))

(defun simple-flee (origin creature)
  (let* ((ox (creature-x origin))
	 (oy (creature-y origin))
	 (best-step (cons 0 0))
	 (best-value (distance ox oy (creature-x creature) (creature-y creature))))
    (dolist (dx '(-1 0 1))
      (dolist (dy '(-1 0 1))
	(let* ((x (+ dx (creature-x creature)))
	       (y (+ dy (creature-y creature)))
	       (dist (distance ox oy x y)))
	  (if (and (> dist best-value)
		   (creature-can-walk? creature
				       (tile-at (object-level creature) x y)))
	      (setf best-step (cons dx dy)
		    best-value dist)))))
    best-step))

(defun install-stateai (creature ai-f &rest rest)
  (setf (creature-ai creature)
	(apply ai-f (cons creature rest))))

(defun emit-ai-message (creature argument default)
  (unless (not (alive? creature))
    (cond ((stringp argument)
	   (emit-visual creature argument))
	  (argument (emit-visual creature default)))))

(defun stateai-harmless-until-provoked (creature delay &key (cooldown-while-visible nil) (enrage-message t) (calm-message t) (enraged-behaviour #'ai-fair-search-player-and-destroy) (calm-behaviour #'ai-random-walk))
  (let ((state :harmless)
	(cooldown nil))
    (install-hook creature
		  :after-attacked
		  #'(lambda (attacker)
		      (unless (not (eq attacker *game-player*))
			(unless (eq state :provoked)
			  (emit-ai-message creature
					   enrage-message
					   (format nil
						   "~a becomes enraged!"
						   (definite-noun (creature-name creature))))))
		      (setf state :provoked
			    cooldown delay)))
    #'(lambda (creature)
	(case state
	  (:harmless (funcall calm-behaviour creature))
	  (:provoked
	   (flet ((dec-cooldown ()
		    (decf cooldown)
		    (if (<= cooldown 0)
			(progn
			  (setf state :harmless)
			  (emit-ai-message creature
					   calm-message
					   (format nil
						   "~a calms down."
						   (definite-noun (creature-name creature))))))))
	     (if cooldown-while-visible
		 (dec-cooldown))
	     (cond ((not (visible-to? *game-player* creature))
		    (unless cooldown-while-visible
		      (dec-cooldown)))
		   ((ai-trigger-imperfection?)
		    (ai-random-walk creature))
		   (t (funcall enraged-behaviour creature)))))))))

(defun stateai-harmless-until-approached (creature delay &key radius (cooldown-while-visible nil) (enrage-message t) (calm-message t) (enraged-behaviour #'ai-fair-search-player-and-destroy) (calm-behaviour #'ai-random-walk))
  (let ((state :harmless)
	(cooldown nil))
    (flet ((provoke ()
	     (unless (eq state :provoked)
	       (emit-ai-message creature
				enrage-message
				(format nil
					"~a becomes enraged!"
					(definite-noun (creature-name creature)))))
	     (setf state :provoked
		   cooldown delay)))
      (install-hook creature
		    :provoke
		    #'(lambda ()
			(debug-print 50 "Provoked by signal!~%")
			(provoke)))
      (install-hook creature
		    :after-attacked
		    #'(lambda (attacker)
			(unless (not (alive? creature))
			  (unless (not (eq attacker *game-player*))
			    (provoke)))))
      #'(lambda (creature)
	  (if (and
	       (<=
		(distance (creature-x creature) (creature-y creature)
			  (creature-x *game-player*) (creature-y *game-player*))
		radius)
	       (visible-to? *game-player* creature))
	      (provoke))
	  (case state
	    (:harmless (funcall calm-behaviour creature))
	    (:provoked
	     (flet ((dec-cooldown ()
		      (decf cooldown)
		      (if (<= cooldown 0)
			  (progn
			    (setf state :harmless)
			    (emit-ai-message creature
					     calm-message
					     (format nil
						     "~a calms down."
						     (definite-noun (creature-name creature))))))))
	       (if cooldown-while-visible
		   (dec-cooldown))
	       (cond ((not (visible-to? *game-player* creature))
		      (unless cooldown-while-visible
			(dec-cooldown)))
		     ((ai-trigger-imperfection?)
		      (ai-random-walk creature))
		     (t (funcall enraged-behaviour creature))))))))))
    
(let ((ai-success-chance (make-chance-roll :success-chance 6/7)))
  (defun ai-trigger-imperfection? ()
    (not (roll-success? ai-success-chance))))

(defun ai-attack-player-if-adjacent (creature &key (otherwise #'ai-random-walk))
  (let ((target *game-player*))
    (cond ((adjacent-to? target creature)
	   (melee-attack target creature))
	  (t
	   (funcall otherwise creature)))))

(defun ai-search-player-and-destroy (creature)
  (let ((target *game-player*))
    (cond ((adjacent-to? target creature)
	   (debug-print 50 "SPD ai: player adjacent~%")
	   (melee-attack target creature))
	  (t
	   (debug-print 50 "SPD ai: seeking path to player~%")
	   (make-step-or-random creature
				(sad-search target creature))))))
  
(defun ai-fair-search-player-and-destroy (creature)
  (let ((target *game-player*))
    (cond ((not (visible-to? target creature))
	   (debug-print 50 "FSPD ai: player not visible~%")
	   (ai-random-walk creature))
	  ((adjacent-to? target creature)
	   (debug-print 50 "FSPD ai: player adjacent~%")
	   (melee-attack target creature))
	  (t
	   (debug-print 50 "FSPD ai: seeking path to player~%")
	   (make-step-or-random creature
				(sad-search target creature))))))

(defun ai-test (creature)
  (cond (*ai-test-fleeing* (make-step-or-random creature
						  (simple-flee *game-player* creature)))
	(t (ai-search-player-and-destroy creature))))
  
  

(defun stabilize-stepmap (array level &optional (limit nil) (is-not-obstacle? #'tile-walkable))
  (let ((queued (remove-if-not #'(lambda (xy) (let ((value (aref array (car xy) (cdr xy))))
						(not (null value))))
			       (all-xys))))
    (do ()
	((null queued) array)
      (let* ((current-xy (pop queued))
	     (value (+ 1 (aref array (car current-xy) (cdr current-xy)))))
	(dolist (neighbour-xy (remove-if-not #'xy-in-bounds? (neighbours-of-xy current-xy)))
	  (unless (and (not (null limit))
		       (>= value limit))
	    (unless (not (funcall is-not-obstacle? (tile-at level (car neighbour-xy) (cdr neighbour-xy))))
	      (unless (and (not (null (aref array (car neighbour-xy) (cdr neighbour-xy))))
			   (<= (aref array (car neighbour-xy) (cdr neighbour-xy))
			       value))
		(setf (aref array (car neighbour-xy) (cdr neighbour-xy)) value)
		(push neighbour-xy queued)))))))))
  
(defmethod get-stepmap-to ((creature creature))
  (with-slots (stepmap-to)
      creature
    (setf stepmap-to
	  (or stepmap-to
	      (let ((arr (make-array (array-dimensions (level-tiles (object-level creature)))
				     :initial-element nil)))
		(setf (aref arr (creature-x creature) (creature-y creature))
		      0)
		(stabilize-stepmap arr (object-level creature) 15))))))

(defmethod make-tactical-stepmap-to ((creature creature) (mover creature))
  (with-slots (stepmap-to)
      creature
    (let ((arr (make-array (array-dimensions (level-tiles (object-level creature)))
			   :initial-element nil)))
      (setf (aref arr (creature-x creature) (creature-y creature))
	    0)
      (stabilize-stepmap arr (object-level creature) 5 #'(lambda (tile) (creature-can-walk? mover tile))))))

(let ((darkness-stepmap nil))
  (defun get-stepmap-to-darkness (level)
    (or darkness-stepmap
	(setf darkness-stepmap
	      (stabilize-stepmap
	       (get-stepmap-base level
				 #'(lambda (tile) (tile-dark tile)))
	       level
	       15))))
  (defun invalidate-stepmap-to-darkness ()
    (setf darkness-stepmap nil)))
