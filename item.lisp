(in-package :light-7drl)

(defun make-item (&rest rest)
  (apply #'make-instance
	 (cons 'item
	       rest)))

(defun make-weapon (&rest rest)
  (apply #'make-item rest))

(defmethod drop-at ((item item) new-level x y)
  (with-slots (level light-source)
      item
    (setf level new-level)
    (unless (null light-source)
      (move-light-source light-source x y))
    (push item (level-floor-items level))
    (push item (tile-items (tile-at level x y)))))

(defmethod picked-up ((item item))
  (with-slots (level)
      item
    (unless (null level)
      (setf (level-floor-items level)
	    (remove item (level-floor-items level))))
    (setf level nil)))

(defmethod radiant-intensity ((radiant radiant))
  (with-slots (light-source)
      radiant
    (if (null light-source)
	0
	(light-source-intensity light-source))))
