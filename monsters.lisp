(in-package :light-7drl)

(defnoun n-glowbug "a" "glow-bug" "glow-bugs")
(defnoun n-rat "a" "rat" "rats")
(defnoun n-snake "a" "viper" "vipers")
(defnoun n-bat "a" "bat" "bats")

(defnoun n-shadow "a" "shadow" "shadows")
(defnoun n-shadow-lord "a" "shadow lord" "shadow lords")

(defnoun n-cave-wolf "a" "cave wolf" "cave wolves")


(defverb-23p v-is-poisoned "are poisoned" "is poisoned")


(defun make-shadow ()
  (let ((rv (make-creature
	     :appearance (make-appearance :glyph (char-code #\s)
					  :foreground-colour '(0 0 0))
	     :name n-shadow
	     :gender nil
	     :darkvision t
	     :hit-chance (make-chance-roll :success-chance 1)
	     :damage (make-dice-roll :number-of-dice 2
				     :dice-size 6)
	     :dodge-multiplier 1
	     :max-hp 20
	     :light-vulnerable t
	     :darkness-regenerating t
	     :ai #'ai-search-player-and-destroy)))
    rv))

(defun make-shadow-lord ()
  (let ((rv (make-creature
	     :appearance (make-appearance :glyph (char-code #\S)
					  :foreground-colour '(0 0 0))
	     :name n-shadow-lord
	     :darkvision t
	     :hit-chance (make-chance-roll :success-chance 1)
	     :damage (make-dice-roll :number-of-dice 4
				     :dice-size 6)
	     :dodge-multiplier 0
	     :max-hp 100
	     :darkness-regenerating t
	     :light-vulnerable t
	     :ai #'ai-boss-shadowlord)))
    (install-hook rv
		  :before-death
		  #'show-game-ending)
    rv))

(defun make-bat ()
  (let ((rv (make-creature
	     :appearance (make-appearance :glyph (char-code #\b)
					  :foreground-colour '(255 255 255))
	     :name n-bat
	     :gender nil
	     :darkvision t
	     :hit-chance (make-chance-roll :success-chance 3/4)
	     :damage (make-dice-roll :number-of-dice 1
				     :dice-size 6)
	     :dodge-multiplier 1/2
	     :max-hp 5)))
    (install-hook rv
		  :bat-signal
		  #'(lambda ()
		      (debug-print 50 "Received bat signal.~%")
		      (run-hooks rv :provoke)))
    (install-hook rv
		  :after-attacked
		  #'(lambda (attacker)
		      (declare (ignore attacker))
		      (debug-print 50 "Being attacked!.~%")
		      (unless (null (object-level rv))
			(dolist (creature (level-creatures (object-level rv)))
			  (debug-print 50 "Sending bat signal -> ~a!.~%" creature)
			  (run-hooks creature :bat-signal)))))
    (install-stateai rv
		     #'stateai-harmless-until-approached
		     10
		     :cooldown-while-visible nil
		     :enrage-message nil
		     :calm-message nil
		     :radius 0
		     :enraged-behaviour #'ai-fair-search-player-and-destroy
		     :calm-behaviour #'ai-seek-darkness)
    rv))

(defun make-snake ()
  (let ((rv (make-creature
	     :appearance (make-appearance :glyph (char-code #\~)
					  :foreground-colour '(25 200 25))
	     :name n-snake
	     :gender nil
	     :hit-chance (make-chance-roll :success-chance 3/4)
	     :damage (make-dice-roll :number-of-dice 1
				     :dice-size 0)
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

(defun make-cave-wolf ()
  (let ((rv (make-creature
	     :appearance (make-appearance :glyph (char-code #\W)
					  :foreground-colour '(200 25 25))
	     :name n-cave-wolf
	     :gender nil
	     :hit-chance (make-chance-roll :success-chance 3/4)
	     :damage (make-dice-roll :number-of-dice 2
				     :dice-size 6)
	     :dodge-multiplier 1
	     :max-hp 15)))
    (install-stateai rv
		     #'stateai-harmless-until-approached
		     10
		     :cooldown-while-visible t
		     :enrage-message nil
		     :calm-message nil
		     :radius 15)
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


(defparameter *default-monster-constructors*
  (list
   (list #'make-bat (make-dice-roll :number-of-dice 1
			       :dice-size 4))
   (list #'make-snake (make-dice-roll :number-of-dice 1
				 :dice-size 2))
   (list #'make-rat (make-dice-roll :number-of-dice 1
			       :dice-size 3))
   (list #'make-glowbug (make-dice-roll :number-of-dice 1
			       :dice-size 1))))
   

