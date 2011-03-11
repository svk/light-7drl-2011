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

(defparameter *game-map* nil)
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
  (push (apply #'format (cons nil rest)) *game-text-buffer*))

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
  (buffer-show "~a [y/n]" question)
  (push-hooks #'(lambda (value stack)
		  (declare (ignore stack))
		  (case value
		    (#\y (funcall f-yes)
			 (pop-hooks))
		    (#\n (unless (null f-no)
			   (funcall f-no))
			 (pop-hooks))
		    (t (buffer-show "~a [y/n]" question))))))


