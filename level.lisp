(in-package :light-7drl)

(defun level-add-delayed-spawn (level n f)
  "Run a spawn method such that we can never disprove that the spawn was to a tile among the last n to be explored."
  (push (list n f) (level-delayed-spawns level)))

(defun level-set-all-unexplored (level)
  (dolist (xy (setf (level-unexplored level)
		      (find-walkables level)))
    (setf (tile-explored (tile-at level (car xy) (cdr xy))) nil)))

(defun not-special? (tile)
  (null (tile-special tile)))

(defun mutate-random-tile (level xy-choices mutate &key (eligible (const t)))
  (setf xy-choices (remove-if-not eligible xy-choices :key #'(lambda (xy) (tile-at level (car xy) (cdr xy)))))
  (unless (null xy-choices)
    (let* ((selection (select-random xy-choices))
	   (x (car selection))
	   (y (cdr selection)))
      (funcall mutate level x y))
    t))

(defun debug-paint-tile (level x y)
  (debug-print 1 "BOOM: tile mutated~%")
  (setf (appearance-background-colour (tile-appearance (tile-at level x y)))
	'(255 0 255)))

(defun place-hole (level x y)
  (let ((tile (tile-at level x y)))
    (setf (tile-appearance tile)
	  (make-appearance :glyph 254
			   :foreground-colour '(0 0 0)
			   :background-colour (appearance-background-colour
					       (tile-appearance tile))))
    (setf (tile-special tile) :hole)))

(defun first-n (n list)
  (cond ((<= n 0) nil)
	(t (cons (car list) (first-n (- n 1) (cdr list))))))

(defun find-eligible-near (eligible level x y)
  (select-random
   (first-n 10
	    (mapcar #'cadr
		    (sort 
		     (mapcar #'(lambda (xy)
				 (list (distance x y (car xy) (cdr xy)) xy))
			     (remove-if-not eligible (all-xys) :key #'(lambda (xy) (tile-at level (car xy) (cdr xy)))))
		     #'<
		     :key #'car)))))

(defun place-lever (level x y)
  (let ((tile (tile-at level x y)))
    (setf (tile-appearance tile)
	  (make-appearance :glyph (char-code #\!)
			   :foreground-colour '(0 0 0)
			   :background-colour (appearance-background-colour
					       (tile-appearance tile))))
    (setf (tile-special tile) :lever)))

(defun level-explore (level xys)
  (let ((rv nil))
    (dolist (xy xys)
      (let ((tile (tile-at level
			   (car xy)
			   (cdr xy))))
	(unless (or (tile-explored tile)
		    (tile-dark tile))
	  (push xy rv)
	  (setf (tile-explored tile) t))))
    (setf (level-unexplored level)
	  (set-difference (level-unexplored level)
			  rv
			  :test #'equal))
    rv))
	

