(in-package :light-7drl)

(defun make-creature (&key appearance name max-hp ai gender damage hit-chance (hp nil) (darkvision nil) (attack-verb v-attack) (miss-verb v-miss) (hit-verb v-hit) (speed 1) (light-intensity nil) (dodge-multiplier 1))
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
		 :dodge-multiplier dodge-multiplier
		 :light-source (if light-intensity
				   (make-light-source
				    :x nil
				    :y nil
				    :intensity light-intensity))))

(defmethod tick ((creature creature))
  (with-slots (speed ai)
      creature
    (dotimes (i speed)
      (funcall ai creature))))

(defmethod emit-light ((creature creature))
  (with-slots (light-source level)
      creature
    (unless (null light-source)
      (debug-print 50 "Creature ~a is emitting light.~%" creature)
      (let ((fov-map (level-acquire-obstacle-map level)))
	(add-light-from-source light-source fov-map)
	(level-release-obstacle-map level fov-map)))))

(defmethod set-position ((creature creature) x y level)
  (debug-print 50 "Creature ~a is on ~a, moving to ~a ~a ~a.~%" creature (creature-level creature) x y level)
  (unless (not (null level))
    (setf level (creature-level creature)))
  (unless (eq (creature-level creature) level)
    (unless (null (creature-level creature))
      (setf (level-creatures (creature-level creature)) (remove creature (level-creatures (creature-level creature)))))
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
	(creature-level creature) level
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
      (apply f rest))))

(defmethod install-hook ((creature creature) hook-name f)
  (with-slots (hooks)
      creature
    (let ((current-hooks (gethash hook-name hooks nil)))
      (setf (gethash hook-name hooks) (cons f current-hooks)))))
  
(defmethod melee-attack ((target creature) (attacker creature))
  (with-slots (base-hit-chance base-damage attack-verb hit-verb miss-verb)
      attacker
    (let ((hit-chance (with-slots (dodge-multiplier)
			  target
			(multiply-chance-by base-hit-chance dodge-multiplier)))
	  (dice-roll base-damage)
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
	       (buffer-show-cap "~a ~a for ~a damage!"
				(dnoun-verbs (third-person attacker) hit-verb)
				(definite-noun t-noun)
				damage)
	       (damage target damage))))
      (run-hooks target :after-attacked attacker))))

(defmethod third-person ((creature creature))
  (with-slots (gender)
      creature
    (cond ((eq :female gender) n-she)
	  ((eq :male gender) n-he)
	  (t n-it))))

(defmethod creature-death-verb ((creature creature))
  v-die)

(defmethod die ((creature creature))
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
  (let ((level (creature-level creature)))
    (debug-print 10 "Removing creature ~a from ~a.~%" creature level)
    (unless (null level)
      (debug-print 10 "Removed creature. Left on map: ~a." (level-creatures level))
      (setf (level-creatures level) (remove creature (level-creatures level))))
    (setf (creature-level creature) nil)
    (debug-print 10 "Did remove creature now at ~a.~%" (creature-level creature))
    (unless (null (creature-tile creature))
      (setf (tile-creature (creature-tile creature)) nil))))

