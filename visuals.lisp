(in-package :light-7drl)

(defun make-centered-text-special-screen (text &optional (fg '(255 255 255)) (bg '(0 0 0)))
  #'(lambda ()
      (tcod:console-set-default-background tcod:*root* (apply #'tcod:compose-colour bg))
      (tcod:console-set-default-foreground tcod:*root* (apply #'tcod:compose-colour fg))
      (tcod:console-clear tcod:*root*)
      (tcod:console-print-rect-ex tcod:*root*
				  (max 0 (truncate (/ (- +screen-width+ (length text)) 2)))
				  (truncate (/ +screen-height+ 2))
				  +screen-width+
				  1
				  :set
				  :left
				  text)
      (tcod:console-flush)))

(defun make-game-over-death-screen (condition)
  (make-centered-text-special-screen "Game over: you died. Better luck next time!" '(128 128 128) '(0 0 0)))

(defun make-game-over-victory-screen (condition)
  (make-centered-text-special-screen "Congratulations! You've won the game!" '(255 255 255) '(128 0 128)))
  
