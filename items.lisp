(in-package :light-7drl)


(defnoun n-knife "a" "knife" "knives")
(defnoun n-sword-of-light "a" "glowing blade" "glowing blades")
(defnoun n-brazier "a" "brazier" "braziers")

(defun make-sword-of-light ()
  (make-weapon :appearance (make-appearance :glyph (char-code #\/)
					    :foreground-colour '(255 255 255))
	       :name n-sword-of-light))

(defun make-brazier ()
  (make-item :appearance (make-appearance :glyph (char-code #\#)
					  :foreground-colour '(255 0 0))
	     :name n-brazier
	     :heavy t
	     :light-source (make-light-source :x nil
					      :y nil
					      :intensity 0.9)))
