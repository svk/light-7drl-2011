(in-package :light-7drl)

(defclass noun ()
  ((indefinite-article :reader noun-indefinite-article
		       :initarg :indefinite-article)
   (use-definite-article :reader noun-use-definite-article
			 :initarg :use-definite-article)
   (singular :reader noun-singular
	     :initarg :singular)
   (plural :reader noun-plural
	   :initform nil
	   :initarg :plural)
   (person :reader noun-person
	   :initform :third
	   :initarg :person)))
  
(defun grammatical-number (count)
  (case count
    (1 :singular)
    (t :plural)))

(defclass verb ()
  ((conjugation-table :initform (make-hash-table :test #'equal))))

(defmethod declare-conjugation ((verb verb) person number tense word)
  (with-slots (conjugation-table)
      verb
    (setf (gethash (list person number tense) conjugation-table) word)))

(defmethod lookup-conjugation ((verb verb) person number tense)
  (with-slots (conjugation-table)
      verb
    (gethash (list person number tense) conjugation-table "[VERB-ERROR]")))

(defmethod dnoun-verbs ((noun noun) (verb verb))
  (format nil
	  "~a ~a"
	  (definite-noun noun)
	  (lookup-conjugation verb (noun-person noun) :singular :present)))

(defmethod inoun-verbs ((noun noun) (verb verb))
  (format nil
	  "~a ~a"
	  (indefinite-noun noun)
	  (lookup-conjugation verb (noun-person noun) :singular :present)))

(defmethod pnoun-verb ((noun noun) (verb verb))
  (format nil
	  "~a ~a"
	  (noun-plural noun)
	  (lookup-conjugation verb (noun-person noun) :plural :present)))

(defmethod icnoun-verb (count (noun noun) (verb verb))
  (format nil
	  "~a ~a"
	  (indefinite-counted-noun noun count)
	  (lookup-conjugation verb (noun-person noun) (grammatical-number count) :present)))
  

(defmacro defnoun (name indefinite-article singular plural &optional (use-definite-article t) (person :third))
    `(defparameter ,name (make-instance 'noun :indefinite-article ,indefinite-article
					:use-definite-article ,use-definite-article
					:singular ,singular
					:plural ,plural
					:person ,person)))

(defmacro defnoun-uncountable (name singular &optional (use-definite-article t) (person :third))
    `(defparameter ,name (make-instance 'noun :indefinite-article nil
					:use-definite-article ,use-definite-article
					:singular ,singular
					:plural nil
					:person ,person)))

(defun make-proper-noun (name)
  (make-instance 'noun :indefinite-article nil
		 :use-definite-article nil
		 :singular name
		 :plural nil))

(defnoun-uncountable n-you "you" :person :second)

(defnoun n-sword "a" "sword" "swords")
(defnoun n-child "a" "child" "children")
(defnoun n-goose "a" "goose" "geese")
(defnoun n-arrow "an" "arrow" "arrows")
(defnoun n-monster "a" "monster" "monsters")
(defnoun-uncountable n-tea "tea")

(defnoun-uncountable n-he "he")
(defnoun-uncountable n-she "she")
(defnoun-uncountable n-it "it")

(defnoun n-creature "a" "creature" "creatures")
(defnoun n-small-creature "a" "small creature" "small creatures")
(defnoun n-large-creature "a" "large creature" "large creatures")

(defmacro defverb-23p (name present-second present-third)
  (let ((sym (gensym)))
    `(defparameter ,name
       (progn (let ((,sym (make-instance 'verb)))
		(declare-conjugation ,sym :second :singular :present ,present-second)
		(declare-conjugation ,sym :third :singular :present ,present-third)
		,sym)))))

;; All in moderation for verbs: (present) you -, he/she/it -
(defverb-23p v-kill "kill" "kills")
(defverb-23p v-strike "strike" "strikes")
(defverb-23p v-die "die" "dies")
(defverb-23p v-miss "miss" "misses")
(defverb-23p v-is "are" "is")
(defverb-23p v-hit "hit" "hits")
(defverb-23p v-lie "lie" "lies")
(defverb-23p v-attack "attack" "attacks")


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

(defun definite-noun (noun)
  (noun-prefix-with-definite-article noun (noun-singular noun)))

(defun indefinite-noun (noun)
  (noun-prefix-with-indefinite-article noun (noun-singular noun)))

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
	(entry (assoc thing rv :test #'equal)  (assoc thing rv :test #'equal)))
       ((null more-things) rv)
    (if entry
	(incf (cdr entry))
	(push (cons thing 1) rv))))

(defun third-person-singular (gender)
  (case gender
    (:female "she")
    (:male "he")
    (t "it")))

(defun third-person-possessive (gender)
  (case gender
    (:female "her")
    (:male "his")
    (t "its")))

(defmethod print-object ((object noun) stream)
  (format stream "[~a]" (noun-singular object)))

(defun list-join (strings)
  (with-output-to-string (out)
    (do ((count (length strings) (- count 1))
	 (more-strings strings (cdr more-strings)))
	((zerop count))
      (format out "~a" (car more-strings))
      (case count
	(1)
	(2 (format out " and "))
	(t (format out ", "))))))

(defun capitalize (string)
  (format nil "~a~a" (char-upcase (aref string 0)) (subseq string 1)))


    

