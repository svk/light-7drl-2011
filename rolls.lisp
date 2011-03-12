(in-package :light-7drl)

(defstruct chance-roll
  success-chance)

(defstruct dice-roll
  (number-of-dice 1)
  dice-size
  (constant 0))

(defmethod print-object ((roll chance-roll) stream)
  (with-slots (success-chance)
      roll
  (format stream "[~,2F%]" (coerce (* 100 success-chance) 'float))))

(defmethod print-object ((roll dice-roll) stream)
  (with-slots (number-of-dice dice-size constant)
      roll
    (if (zerop constant)
	 (format stream
		 "[~ad~a]" number-of-dice dice-size)
	 (format stream
		 "[~ad~a~@d]" number-of-dice dice-size constant))))

