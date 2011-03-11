(in-package :light-7drl)

(defun make-creature (&key appearance name max-hp ai gender (hp nil))
  (make-instance 'creature
		 :appearance appearance
		 :name name
		 :hp (if (null hp) max-hp hp)
		 :max-hp max-hp
		 :gender gender
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
		  (level-release-obstacle-map (creature-level level) fov-map)
		  rv))))))

(defmethod is-in-fov? ((creature creature) fov)
  (with-slots (xy)
      creature
    (aref fov (car xy) (cdr xy))))

(defmethod has-darkvision? ((creature creature))
  nil)

(defmethod visible-to? ((target creature) (observer creature))
  (and (or (not (tile-dark (creature-tile target)))
	   (has-darkvision? observer))
       (is-in-fov? target (get-fov observer))))

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








