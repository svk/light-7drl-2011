(in-package :light-7drl)

(defun make-creature (&key appearance name max-hp ai gender damage hit-chance (hp nil) (darkvision nil) (attack-verb v-attack) (miss-verb v-miss) (hit-verb v-hit) (speed 1) (light-intensity nil) (dodge-multiplier 1) (attack-inflicts-status nil))
  (make-instance 'creature
		 :appearance appearance
		 :name name
		 :hp (if (null hp) max-hp hp)
		 :max-hp max-hp
		 :gender gender
		 :darkvision darkvision
		 :ai ai
		 :damage damage
		 :hit-chance hit-chance
		 :attack-verb attack-verb
		 :miss-verb miss-verb
		 :hit-verb hit-verb
		 :speed speed
		 :attack-inflicts-status attack-inflicts-status
		 :dodge-multiplier dodge-multiplier
		 :light-source (if light-intensity
				   (make-light-source
				    :x nil
				    :y nil
				    :intensity light-intensity))))

(defmethod remove-status ((creature creature) type)
  (with-slots (statuses)
      creature
    (setf statuses
	  (remove-if #'(lambda (x) (equal x type)) statuses :key #'identity))))


(defmethod get-status ((creature creature) type)
  (with-slots (statuses)
      creature
    (find type statuses :key #'identity)))

(defmethod tick ((creature creature))
  (with-slots (speed ai)
      creature
    (unless (null ai)
      (dotimes (i speed)
	(funcall ai creature))))
  (dolist (item (creature-items creature))
    (tick item))
  (unless (not (get-status creature :poison))
    (damage creature 1)))

(defmethod set-position ((creature creature) x y level)
  (debug-print 50 "Creature ~a is on ~a, moving to ~a ~a ~a.~%" creature (object-level creature) x y level)
  (unless (not (null level))
    (setf level (object-level creature)))
  (unless (eq (object-level creature) level)
    (unless (null (object-level creature))
      (setf (level-creatures (object-level creature)) (remove creature (level-creatures (object-level creature)))))
    (pushnew creature (level-creatures level)))
  (unless (null (creature-tile creature))
    (setf (tile-creature (creature-tile creature)) nil))
  (invalidate-fov creature)
  (with-slots (light-source)
      creature
    (unless (null light-source)
      (move-light-source light-source x y)))
  (with-slots (stepmap-to)
      creature
    (setf stepmap-to nil))
  (setf (tile-creature (tile-at level x y)) creature
	(object-level creature) level
	(creature-xy creature) (cons x y)
	(creature-tile creature) (tile-at level x y)))

(defmethod invalidate-fov ((creature creature))
  (with-slots (fov)
      creature
    (setf fov nil)))

(defmethod get-fov ((creature creature))
  (with-slots (level fov xy)
      creature
    (setf fov
	  (or fov
	      (let ((fov-map (level-acquire-obstacle-map level)))
		(tcod:map-compute-fov fov-map
				      (car xy)
				      (cdr xy)
				      (max (level-width level) (level-height level))
				      t
				      :fov-shadow)
		(let ((rv (extract-fov fov-map)))
		  (level-release-obstacle-map level fov-map)
		  rv))))))

(defmethod is-in-fov? ((creature creature) fov)
  (with-slots (xy)
      creature
    (aref fov (car xy) (cdr xy))))

(defun xys-adjacent? (xy0 xy1)
  (let ((dx (- (car xy0) (car xy1)))
	(dy (- (cdr xy0) (cdr xy1))))
    (and (<= (abs dx) 1)
	 (<= (abs dy) 1))))

(defmethod adjacent-to? ((alpha creature) (beta creature))
  (xys-adjacent? (creature-xy alpha)
		 (creature-xy beta)))
  

(defmethod has-darkvision? ((creature creature))
  (with-slots (darkvision) creature darkvision))

(defmethod alive? ((creature creature))
  (with-slots (hp)
      creature
    (> hp 0)))

(defmethod radiant-intensity ((creature creature))
  (with-slots (light-source items)
      creature
    (+ (if light-source
	   (light-source-intensity light-source)
	   0)
       (apply #'+ (mapcar #'radiant-intensity items)))))

(defmethod emit-light ((creature creature))
  (with-slots (light-source level items xy)
      creature
    (let ((temp-light (make-light-source :x (car xy)
					 :y (cdr xy)
					 :intensity (radiant-intensity creature))))
      (debug-print 50 "Creature ~a is emitting light.~%" creature)
      (let ((fov-map (level-acquire-obstacle-map level)))
	(add-light-from-source temp-light fov-map)
	(level-release-obstacle-map level fov-map)))))

(defmethod visible-to? ((target creature) (observer creature))
  (or (eq target observer)
      (and (or (not (tile-dark (creature-tile target)))
	       (has-darkvision? observer))
	   (is-in-fov? target (get-fov observer)))))

(defmethod emit-visual ((creature creature) message)
  (unless (not (visible-to? creature *game-player*))
    (buffer-show-cap message)))

(defmethod run-hooks ((creature creature) hook-name &rest rest)
  (with-slots (hooks)
      creature
    (dolist (f (gethash hook-name hooks nil))
      (debug-print 50 "Running hook on ~a ~a: ~a~%" creature hook-name f)
      (apply f rest))))

(defmethod install-hook ((creature creature) hook-name f)
  (with-slots (hooks)
      creature
    (let ((current-hooks (gethash hook-name hooks nil)))
      (setf (gethash hook-name hooks) (cons f current-hooks))
      (debug-print 50 "Hooks now installed on ~a ~a: ~a~%" creature hook-name (gethash hook-name hooks)))))

(defmethod get-base-hit-chance ((creature creature))
  (with-slots (items weapon base-hit-chance)
      creature
    (if (null weapon)
	base-hit-chance
	(if (find weapon items)
	    (first (item-attack-power weapon))
	    (progn
	      (setf weapon nil)
	      base-hit-chance)))))

(defmethod get-base-damage ((creature creature))
  (with-slots (items weapon base-damage)
      creature
    (if (null weapon)
	base-damage
	(if (find weapon items)
	    (second (item-attack-power weapon))
	    (progn
	      (setf weapon nil)
	      base-damage)))))
  
(defmethod melee-attack ((target creature) (attacker creature))
  (with-slots (base-hit-chance base-damage attack-verb hit-verb miss-verb attack-inflicts-status)
      attacker
    (let ((hit-chance (with-slots (dodge-multiplier)
			  target
			(multiply-chance-by (get-base-hit-chance attacker) dodge-multiplier)))
	  (dice-roll (get-base-damage attacker))
	  (player-visible (or (visible-to? target *game-player*)
			      (visible-to? attacker *game-player*)))
	  (t-noun (creature-name target))
	  (a-noun (creature-name attacker)))
      (setf player-visible t)
      (unless (not player-visible)
	(buffer-show-cap "~a ~a! (~a ~a)"
			 (dnoun-verbs a-noun attack-verb)
			 (definite-noun t-noun)
			 hit-chance
			 dice-roll))
      (cond ((not (roll-success? hit-chance))
	     (buffer-show-cap "~a." (dnoun-verbs (third-person attacker)
						 miss-verb)))
	    (t
	     (let ((damage (roll-result? dice-roll)))
	       (if (> damage 0)
		   (buffer-show-cap "~a ~a for ~a damage!"
				    (dnoun-verbs (third-person attacker) hit-verb)
				    (definite-noun t-noun)
				    damage)
		   (buffer-show-cap "~a ~a!"
				    (dnoun-verbs (third-person attacker) hit-verb)
				    (definite-noun t-noun)
				    damage))
	       (unless (null attack-inflicts-status)
		 (inflict-status target attack-inflicts-status))
	       (damage target damage))))
      (run-hooks target :after-attacked attacker))))

(defmethod inflict-status ((creature creature) status-attack)
  (with-slots (statuses)
      creature
    (unless (find (status-attack-type status-attack) statuses)
      (push (status-attack-type status-attack) statuses)
      (emit-visual creature
		   (format nil
			   "~a!"
			   (dnoun-verbs (creature-name creature)
					(status-attack-verb status-attack)))))))

(defmethod third-person ((creature creature))
  (with-slots (gender)
      creature
    (cond ((eq :female gender) n-she)
	  ((eq :male gender) n-he)
	  (t n-it))))

(defmethod creature-death-verb ((creature creature))
  v-die)

(defmethod die ((creature creature))
  (dolist (item (creature-items creature))
    (drop-at item
	     (object-level creature)
	     (creature-x creature)
	     (creature-y creature)))
  (setf (creature-items creature) nil)
  (run-hooks creature :before-death)
  (remove-from-map creature)
  (unless (not (visible-to? creature *game-player*))
    (buffer-show-cap "~a!" (dnoun-verbs (creature-name creature)
					(creature-death-verb creature)))))
  
(defmethod damage ((creature creature) pts)
  (with-slots (hp)
      creature
    (decf hp pts)
    (unless (> hp 0)
      (die creature))))
  

(defmethod remove-from-map ((creature creature))
  (let ((level (object-level creature)))
    (debug-print 10 "Removing creature ~a from ~a.~%" creature level)
    (unless (null level)
      (debug-print 10 "Removed creature. Left on map: ~a." (level-creatures level))
      (setf (level-creatures level) (remove creature (level-creatures level))))
    (setf (object-level creature) nil)
    (debug-print 10 "Did remove creature now at ~a.~%" (object-level creature))
    (unless (null (creature-tile creature))
      (setf (tile-creature (creature-tile creature)) nil))))

