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

(defun multiply-chance-by (croll scalar)
  (make-chance-roll
   :success-chance (* scalar (chance-roll-success-chance croll))))
	

(defmethod print-object ((roll dice-roll) stream)
  (with-slots (number-of-dice dice-size constant)
      roll
    (if (zerop constant)
	 (format stream
		 "[~ad~a]" number-of-dice dice-size)
	 (format stream
		 "[~ad~a~@d]" number-of-dice dice-size constant))))

(defun roll-success? (chance-roll)
  (<= (random 1.0) (chance-roll-success-chance chance-roll)))

(defun roll-result? (dice-roll)
  (with-slots (number-of-dice dice-size constant)
      dice-roll
    (max 0
	 (if (<= dice-size 0)
	     constant
	     (let ((rv constant))
	       (dotimes (i number-of-dice)
		 (incf rv (+ 1 (random dice-size))))
	       rv)))))
