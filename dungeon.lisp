(in-package :light-7drl)

(defconstant +sketch-floor+ 0)
(defconstant +sketch-wall+ 1)

(defun list-lt (alpha beta)
  (cond ((and (null alpha) (null beta)) nil)
	((null alpha) t)
	((null beta) nil)
	((< (car alpha) (car beta)) t)
	((> (car alpha) (car beta)) nil)
	(t (list-lt (cdr alpha) (cdr beta)))))

(defun add-sorted (item items &key (compare #'<))
  (cond ((null items)
	 (list item))
	((funcall compare item (car items))
	 (cons item items))
	((not (funcall compare (car items) item))
	 items)
	(t (cons (car items) (add-sorted item (cdr items) :compare compare)))))

(defmacro pushnew-sorted (item items &key (compare #'<))
  `(setf ,items (add-sorted ,item ,items :compare ,compare)))

(defun merge-two-sorted-lists (alpha beta &key (compare #'<))
  (cond ((null alpha) beta)
	((null beta) alpha)
	((funcall compare (car alpha) (car beta))
	 (cons (car alpha) (merge-two-sorted-lists (cdr alpha) beta :compare compare)))
	((funcall compare (car beta) (car alpha))
	 (cons (car beta) (merge-two-sorted-lists alpha (cdr beta) :compare compare)))
	(t
	 (cons (car alpha) (merge-two-sorted-lists (cdr alpha) (cdr beta) :compare compare)))))
  
(defun drunken-walk (length)
  (let ((x 0)
	(y 0)
	(rv nil))
    (dotimes (i length)
      (pushnew-sorted (list x y) rv :compare #'list-lt)
      (let ((dxdy (select-random *cardinal-directions*)))
	(incf x (car dxdy))
	(incf y (cdr dxdy))))
    rv))

(defun drunken-walks (length walks)
  (let ((rv nil))
    (dotimes (i walks)
      (setf rv (merge-two-sorted-lists rv (drunken-walk length) :compare #'list-lt)))
    rv))

(defun get-extremes (coordinates)
  (let ((xs (mapcar #'car coordinates))
	(ys (mapcar #'cadr coordinates)))
    (list (apply #'min xs)
	  (apply #'min ys)
	  (apply #'max xs)
	  (apply #'max ys))))

(defun coordinates->array (coordinates &optional (host-width nil) (host-height nil))
  (let* ((rect (get-extremes coordinates))
	 (x0 (nth 0 rect))
	 (y0 (nth 1 rect))
	 (x1 (nth 2 rect))
	 (y1 (nth 3 rect))
	 (width (+ (- x1 x0) 1))
	 (height (+ (- y1 y0) 1))
	 (x-padding (if (null host-width)
			0
			(max 0 (truncate (/ (- host-width width) 2)))))
	 (y-padding (if (null host-height)
			0
			(max 0 (truncate (/ (- host-height height) 2)))))
	 (rv (make-array (list (if (null host-width)
				   width
				   (max host-width width))
			       (if (null host-height)
				   height
				   (max host-height height)))
			 :initial-element +sketch-wall+)))
    (dolist (coordinate coordinates)
      (setf (aref rv
		  (+ x-padding (- (car coordinate) x0))
		  (+ y-padding (- (cadr coordinate) y0)))
	    +sketch-floor+))
    rv))

(defun cons->coordinate (xy)
  (list (car xy) (cdr xy)))

(defun flood-fill (array x y type)
  (unless (not (= type (aref array x y)))
    (do* ((arr (make-array (array-dimensions array) :initial-element nil))
	  (rv nil)
	  (q (list (cons x y)))
	  (qe (pop q) (pop q)))
	 ((null qe) (mapcar #'cons->coordinate rv))
      (setf (aref arr (car qe) (cdr qe)) t)
      (push qe rv)
      (dolist (nxy (cardinal-neighbours-of (car qe) (cdr qe)))
	(unless (or (not (array-in-bounds-p array (car nxy) (cdr nxy)))
		    (not (= type (aref array (car nxy) (cdr nxy))))
		    (aref arr (car nxy) (cdr nxy)))
	  (setf (aref arr (car nxy) (cdr nxy)) t)
	  (push nxy q))))))

(defun merge-sketch-tiles (alpha beta)
  (if (or (= alpha +sketch-floor+)
	  (= beta +sketch-floor+))
      +sketch-floor+
      +sketch-wall+))

(defun find-coordinates (array type)
  (let ((rv nil))
    (dotimes (x (car (array-dimensions array)))
      (dotimes (y (cadr (array-dimensions array)))
	(unless (not (= type (aref array x y)))
	  (push (list x y) rv))))
    rv))

(defun find-regions (array type)
  (let ((lists nil)
	(arr (make-array (array-dimensions array) :initial-element nil)))
    (dolist (coord (find-coordinates array type))
      (unless (not (null (aref arr (car coord) (cadr coord))))
	(push (flood-fill array (car coord) (cadr coord) type) lists)
	(dolist (covered-coord (car lists))
	  (setf (aref arr (car covered-coord) (cadr covered-coord)) t))))
    lists))

(defun largest-region (array type)
  (let ((regions (find-regions array type)))
    (if (null regions)
	nil
	(let* ((max-length (apply #'max (mapcar #'length regions)))
	       (max-regions (remove-if-not #'(lambda (region) (= (length region) max-length)) regions)))
	  (select-random max-regions)))))

(defun pollock-generator (width height target room-gen)
  (do* ((arr (make-array (list width height) :initial-element +sketch-wall+))
	(sampled-area (largest-region arr +sketch-floor+)
		      (largest-region arr +sketch-floor+)))
       ((>= (/ (length sampled-area) (* width height))
	    target)
	sampled-area)
    (let* ((room (coordinates->array (funcall room-gen)))
	   (room-width (car (array-dimensions room)))
	   (room-height (cadr (array-dimensions room)))
	   (x0 (random (+ 1 (- width room-width))))
	   (y0 (random (+ 1 (- height room-height)))))
      (dotimes (dx room-width)
	(dotimes (dy room-height)
	  (setf (aref arr (+ x0 dx) (+ y0 dy))
		(merge-sketch-tiles
		 (aref arr (+ x0 dx) (+ y0 dy))
		 (aref room dx dy))))))))

(defun generate-cave (width height)
  (coordinates->array
   (pollock-generator width height 1/2 #'(lambda () (drunken-walks 10 10)))
   width
   height))