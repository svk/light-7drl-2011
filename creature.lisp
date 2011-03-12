(in-package :light-7drl)

(defun make-creature (&key appearance name max-hp ai gender (hp nil) (darkvision nil))
  (make-instance 'creature
		 :appearance appearance
		 :name name
		 :hp (if (null hp) max-hp hp)
		 :max-hp max-hp
		 :gender gender
		 :darkvision darkvision
		 :ai ai))

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
  
(defmethod melee-attack ((target creature) (attacker creature))
  (let ((hit-chance (make-chance-roll :success-chance 3/4))
	(dice-roll (make-dice-roll :number-of-dice 2
				   :dice-size 4))
	(player-visible (or (visible-to? target *game-player*)
			    (visible-to? attacker *game-player*)))
	(t-noun (creature-name target))
	(a-noun (creature-name attacker)))
    (setf player-visible t)
    (unless (not player-visible)
      (buffer-show-cap "~a ~a! (~a ~a)"
		       (dnoun-verbs a-noun v-attack)
		       (definite-noun t-noun)
		       hit-chance
		       dice-roll))
    (cond ((not (roll-success? hit-chance))
	   (buffer-show-cap "~a." (dnoun-verbs (third-person attacker)
					       v-miss)))
	  (t
	   (let ((damage (roll-result? dice-roll)))
	     (buffer-show-cap "~a ~a for ~a damage!"
			      (dnoun-verbs (third-person attacker) v-strike)
			      (definite-noun t-noun)
			      damage)
	     (damage target damage))))))

(defmethod third-person ((creature creature))
  (with-slots (gender)
      creature
    (cond ((eq :female gender) n-she)
	  ((eq :male gender) n-he)
	  (t n-it))))

(defmethod creature-death-verb ((creature creature))
  v-die)

(defmethod damage ((creature creature) pts)
  (with-slots (hp)
      creature
    (let ((player-visible (visible-to? creature *game-player*)))
      (decf hp pts)
      (unless (> hp 0)
	(remove-from-map creature)
	(unless (not player-visible)
	  (buffer-show-cap "~a!" (dnoun-verbs (creature-name creature)
					      (creature-death-verb creature))))))))
  

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

