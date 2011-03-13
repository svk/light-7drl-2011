(in-package :light-7drl)

(defnoun n-glowbug "a" "glow-bug" "glow-bugs")
(defnoun n-rat "a" "rat" "rats")
(defnoun n-snake "a" "viper" "vipers")

(defverb-23p v-is-poisoned "are poisoned" "is poisoned")

(defun make-snake ()
  (let ((rv (make-creature
	     :appearance (make-appearance :glyph (char-code #\~)
					  :foreground-colour '(25 200 25))
	     :name n-snake
	     :gender nil
	     :hit-chance (make-chance-roll :success-chance 3/4)
	     :damage (make-dice-roll :number-of-dice 1
				     :dice-size 0)
	     :dodge-multiplier 1/2
	     :attack-inflicts-status (make-status-attack :type :poison
							 :verb v-is-poisoned)
	     :max-hp 5)))
    (install-stateai rv
		     #'stateai-harmless-until-approached
		     10
		     :cooldown-while-visible t
		     :enrage-message (format nil "~a hisses loudly!" (definite-noun n-snake))
		     :calm-message (format nil "~a calms down." (definite-noun n-snake))
		     :radius 4
		     :enraged-behaviour #'ai-attack-player-if-adjacent)
    rv))

(defun make-rat ()
  (let ((rv (make-creature
	     :appearance (make-appearance :glyph (char-code #\.)
					  :foreground-colour '(200 25 25))
	     :name n-rat
	     :gender nil
	     :hit-chance (make-chance-roll :success-chance 3/4)
	     :damage (make-dice-roll :number-of-dice 1
				     :dice-size 4)
	     :dodge-multiplier 1/2
	     :max-hp 5)))
    (install-stateai rv
		     #'stateai-harmless-until-approached
		     10
		     :cooldown-while-visible t
		     :enrage-message nil
		     :calm-message nil
		     :radius 4)
    rv))

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
