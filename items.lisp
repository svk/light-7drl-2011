(in-package :light-7drl)


(defnoun n-knife "a" "pocket knife" "pocket knives")
(defnoun n-sword-of-light "a" "glowing blade" "glowing blades")
(defnoun n-brazier "a" "brazier" "braziers")
(defnoun n-charmed-blade "a" "charmed smallsword" "charmed smallswords")

(defnoun n-tinderbox "a" "tinderbox" "tinderboxes")
(defnoun n-torch "a" "torch" "torches")

(defnoun n-healing-potion "a" "red potion" "red potions")
(defnoun n-antidote-potion "a" "blue potion" "blue potions")

(defnoun n-fireproof "a" "fireproof blanket" "fireproof blankets")

(defparameter *fist-power* (list (make-chance-roll :success-chance 1/2)
				 (make-dice-roll :number-of-dice 1
						 :dice-size 6)))
(defparameter *knife-power* (list (make-chance-roll :success-chance 3/4)
				  (make-dice-roll :number-of-dice 2
						  :dice-size 6)))

(defparameter *dagger-power* (list (make-chance-roll :success-chance 3/4)
				  (make-dice-roll :number-of-dice 2
						  :dice-size 6
						  :constant 3)))

;; This glowing is not necessarily just an advantage
(defparameter *sol-power* (list (make-chance-roll :success-chance 4/5)
				(make-dice-roll :number-of-dice 4
						:dice-size 6
						:constant 3)))

(defparameter *enchanted-sword-power* (list (make-chance-roll :success-chance 1)
					    (make-dice-roll :number-of-dice 2
							    :dice-size 6)))
							    

(defparameter *slingshot-power* (list (make-chance-roll :success-chance 3/4)
				      (make-dice-roll :number-of-dice 2
						      :dice-size 4)))

;; The revolver is a "glass sword" -- six shots only. As such more or less
;; an instakill.
(defparameter *revolver-power* (list (make-chance-roll :success-chance 3/4)
				     (make-dice-roll :number-of-dice 6
						     :dice-size 24)))

(defun make-sword-of-light ()
  (make-item :appearance (make-appearance :glyph (char-code #\/)
					  :foreground-colour '(255 255 255))
	     :type :melee-weapon
	     :attack-power *sol-power*
	     :name n-sword-of-light))

(defun make-knife ()
  (make-item :appearance (make-appearance :glyph (char-code #\/)
					  :foreground-colour '(255 255 255))
	     :type :melee-weapon
	     :attack-power *knife-power*
	     :name n-knife))


(defun make-healing-potion ()
  (make-item :appearance (make-appearance :glyph (char-code #\&)
					  :foreground-colour '(255 0 0))
	     :type :healing-potion
	     :name n-healing-potion))

(defun make-antidote-potion ()
  (make-item :appearance (make-appearance :glyph (char-code #\&)
					  :foreground-colour '(0 0 255))
	     :type :antidote-potion
	     :name n-antidote-potion))

(defun make-tinderbox ()
  (make-item :appearance (make-appearance :glyph (char-code #\+)
					  :foreground-colour '(200 150 20))
	     :type :tinderbox
	     :name n-tinderbox))

(defun make-torch ()
  (make-item :appearance (make-appearance :glyph (char-code #\+)
					  :foreground-colour '(200 150 20))
	     :type :torch
	     :name n-torch
	     :ammo +torch-ammo+
	     :active nil))

(defun make-brazier (&key (status nil))
  (make-item :appearance (make-appearance :glyph (char-code #\#)
					  :foreground-colour '(255 0 0))
	     :type :brazier
	     :name n-brazier
	     :heavy t
	     :active (case status
		       (:lit t)
		       (:unlit nil)
		       (nil (select-random '(t nil))))
	     :light-source (make-light-source :x nil
					      :y nil
					      :intensity 0.5)))
