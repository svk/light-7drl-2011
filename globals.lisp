(in-package light-7drl)

(eval-when (:compile-toplevel)
  (defparameter *debug-level* 50))

(defmacro debug-print (level &rest rest)
  (if (and (boundp '*debug-level*)
	   (<= level *debug-level*))
      `(format t ,@rest)
      nil))

(defconstant +wall-glyph+ 219)
(defconstant +player-glyph+ (char-code #\@))
(defconstant +floor-glyph+ (char-code #\ ))

(defconstant +screen-width+ 60)
(defconstant +screen-height+ 40)

(defparameter *tileset-file* "my-tiles.png")

(defconstant +ui-top-lines+ 3)
(defconstant +ui-bottom-lines+ 1)

(defparameter *game-map* nil)
(defparameter *game-player* nil)
(defparameter *game-running* nil)
(defparameter *game-text-buffer* nil)

(defparameter *game-initialized* nil)

(defparameter *game-input-hooks* nil)

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

(defun buffer-show (&rest rest)
  (push (apply #'format (cons nil rest)) *game-text-buffer*))


