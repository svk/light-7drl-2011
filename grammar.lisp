(in-package :light-7drl)

(defstruct noun
  indefinite-article
  use-definite-article
  singular
  plural)

(defmacro defnoun (name indefinite-article singular plural &optional (use-definite-article t))
    `(defparameter ,name (make-noun :indefinite-article ,indefinite-article
			      :use-definite-article ,use-definite-article
			      :singular ,singular
			      :plural ,plural)))

(defmacro defnoun-uncountable (name singular &optional (use-definite-article t))
    `(defparameter ,name (make-noun :indefinite-article nil
			      :use-definite-article ,use-definite-article
			      :singular ,singular
			      :plural nil)))


(defnoun n-sword "a" "sword" "swords")
(defnoun n-child "a" "child" "children")
(defnoun n-goose "a" "goose" "geese")
(defnoun n-arrow "an" "arrow" "arrows")
(defnoun-uncountable n-tea "tea")

(defun noun-definite-article (noun)
  (if (noun-use-definite-article noun)
      "the"
      nil))

(defun english-number (num)
  (if (and (>= num 0) (< num 20))
      (format nil "~r" num) ;; This being in the base language is pretty crazy, but handy now
      (format nil "~a" num)))

(defun noun-prefix-with-indefinite-article (noun string)
  (cond ((not (noun-indefinite-article noun)) string)
	(t (format nil "~a ~a" (noun-indefinite-article noun) string))))
	
(defun noun-prefix-with-definite-article (noun string)
  (cond ((not (noun-definite-article noun)) string)
	(t (format nil "~a ~a" (noun-definite-article noun) string))))

(defun indefinite-counted-noun (noun count &optional (force-numbers nil))
  (cond ((and (not force-numbers) (= count 1))
	 (noun-prefix-with-indefinite-article noun (noun-singular noun)))
	((= count 1)
	 (format nil "~a ~a" (english-number count) (noun-singular noun)))
	((= count 0)
	 (format nil "no ~a" (noun-plural noun)))
	(t
	 (format nil "~a ~a" (english-number count) (noun-plural noun)))))

(defun definite-counted-noun (noun count &optional (force-numbers nil))
  (cond ((and (not force-numbers) (= count 1))
	 (noun-prefix-with-definite-article noun (noun-singular noun)))
	((= count 1)
	 (format nil "~a ~a ~a" (noun-definite-article noun) (english-number count) (noun-singular noun)))
	((= count 0)
	 (format nil "the absence of ~a" (noun-plural noun)))
	(t
	 (noun-prefix-with-definite-article noun (format nil "~a ~a" (english-number count) (noun-plural noun))))))

(defun is-are (count)
  (if (= count 1)
      "is"
      "are"))

(defun debug-conjugate-noun (noun)
  (dolist (i '(1 10 50 0))
    (format t "There ~a ~a here.~%" (is-are i) (indefinite-counted-noun noun i))
    (format t "You pick up ~a.~%" (definite-counted-noun noun i))))

(defun make-count-list (things)
  (do* ((rv nil)
	(more-things things (cdr more-things))
	(thing (car more-things) (car more-things))
	(entry (assoc thing rv :test #'equal)))
       ((null more-things) rv)
    (if entry
	(incf (cdr entry))
	(push (cons thing 1) rv))))
    

