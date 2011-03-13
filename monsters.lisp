(in-package :light-7drl)

(defun make-glowbug ()
  (let ((rv (make-creature
	     :appearance (make-appearance :glyph (char-code #\.)
					  :foreground-colour '(200 200 25))
	     :name n-glowbug
	     :gender nil
	     :hit-chance (make-chance-roll :success-chance 1)
	     :damage (make-dice-roll :number-of-dice 1
				     :dice-size 1)
	     :dodge-multiplier 1/3
	     :light-intensity 0.1
	     :max-hp 5)))
    (install-stateai rv
		     #'stateai-harmless-until-provoked
		     10
		     :cooldown-while-visible t
		     :enrage-message nil
		     :calm-message nil)
    rv))

				
  