(in-package light-7drl)

;; (defmacro debug-print (level &rest rest)
;;  (if (and (boundp '*debug-level*)
;;	   (<= level *debug-level*))
;;      `(format t ,@rest)
;;      nil))

(defparameter *debug-level* 50)

(defun debug-print (level &rest rest)
  (unless (not (and (boundp '*debug-level*)
		    (<= level *debug-level*)))
    (apply #'format (cons t rest))))

(defconstant +weapon-glyph+ (char-code #\/))
(defconstant +wall-glyph+ 219)
(defconstant +player-glyph+ (char-code #\@))
(defconstant +floor-glyph+ (char-code #\ ))

(defconstant +screen-width+ 60)
(defconstant +screen-height+ 40)

(defparameter *tileset-file* "my-tiles.png")

(defconstant +ui-top-lines+ 3)
(defconstant +ui-bottom-lines+ 2)

(defparameter *game-current-level* nil)

(defparameter *game-player* nil)
(defparameter *game-running* nil)
(defparameter *game-text-buffer* nil)

(defparameter *game-initialized* nil)

(defparameter *game-torch* nil)
(defparameter *game-brazier* nil)
(defparameter *game-creatures* nil)

(defconstant +light-visibility-threshold+ (/ 1 255))
(defconstant +minimum-fg-light+ 0.3)
(defconstant +minimum-touched-light+ 0.1)

(defparameter *cheat-lightall* nil)

(defparameter *directions* (list
			     (cons -1 -1) (cons 0 -1) (cons 1 -1)
			     (cons -1 0)              (cons 1 0)
			     (cons -1 1)  (cons 0 1)  (cons 1 1)))

(defun light-half-life (steps)
  "Calculate the factor of (exponential) decay required for light intensity to halve at a distance of steps."
  (/ (- (log 2)) steps))

(defparameter *universal-light-half-life* (light-half-life 4))

(defparameter *game-input-hooks-stack* nil)

(defun buffer-show (&rest rest)
  (let ((string (apply #'format (cons nil rest))))
    (if (equal (caar *game-text-buffer*) string)
	(incf (cdar *game-text-buffer*))
	(push (cons string 1) *game-text-buffer*))))

(defparameter *buffer-clear-time* nil)

(defun buffer-clear ()
  (setf *game-text-buffer* nil
	*buffer-clear-time* nil))

(defun push-hooks (hooks)
  (debug-print 40 "Pushing new hooks.~%")
  (push hooks *game-input-hooks-stack*)
  (debug-print 45 "Hook layers: ~a.~%" (length *game-input-hooks-stack*)))

(defun pop-hooks ()
  (debug-print 40 "Popping hooks.")
  (pop *game-input-hooks-stack*)
  (debug-print 45 "Hook layers: ~a.~%" (length *game-input-hooks-stack*)))

(defun query-confirm (question f-yes &optional (f-no nil))
  (debug-print 10 "Asking to confirm query: ~a~%" question)
  (buffer-clear)
  (buffer-show "~a [y/n]" question)
  (push-hooks #'(lambda (value stack)
		  (declare (ignore stack))
		  (case value
		    (#\y (buffer-clear)
			 (funcall f-yes)
			 (pop-hooks))
		    (#\n (buffer-clear)
			 (unless (null f-no)
			   (funcall f-no))
			 (pop-hooks))
		    (t
		     (buffer-clear)
		     (buffer-show "~a [y/n]" question))))))

(defun any-nonempty (s)
  (if (zerop (length s))
      nil
      s))

(defun query-string (description f-string &key (filter #'identity) (limit nil) (accept #'any-nonempty))
  (let ((buffer nil))
    (defun make-buffer-string ()
      (let ((rv (make-array 0
			    :element-type 'character
			    :fill-pointer 0
			    :adjustable t)))
	(dolist (ch (reverse buffer))
	  (vector-push-extend ch rv))
	rv))
    (debug-print 50 "Querying for string (~a).~%" description)
    (buffer-show "~a~a_" description (make-buffer-string))
    (push-hooks #'(lambda (value stack)
		    (let ((exiting nil))
		      (declare (ignore stack))
		      (setf value (funcall filter value))
		      (cond 
			((eq value :backspace)
			 (pop buffer))
			((eq value :enter)
			 (let ((s (reverse buffer)))
			   (unless (not (funcall accept s))
			     (pop-hooks)
			     (buffer-clear)
			     (setf exiting t)
			     (funcall f-string (make-buffer-string)))))
			((characterp value)
			 (push value buffer))
			(t (debug-print 50 "Going unhandled in query: ~a.~%" value)
			 (debug-print 100 "Vs backspace: ~a ~a ~a ~a~%" value 'backspace (eq value 'backspace) (type-of value))))
		      (unless exiting
			(buffer-clear)
			(buffer-show "~a~a_" description (make-buffer-string))))))))