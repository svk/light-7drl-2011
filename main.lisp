(require :asdf)
(asdf:operate 'asdf:load-op :cffi)
(asdf:operate 'asdf:load-op :tcod)

(in-package :light-7drl)

(cffi:define-foreign-library libtcod
    (t (:default "libtcod")))

(defun terminate ()
  (handler-case
      (progn
	(defcfun "TCOD_sys_term" :void)
	(TCOD-sys-term))
    (sb-kernel::undefined-alien-function-error ())))

(defun load-libraries ()
  (cffi:load-foreign-library 'libtcod :search-path "libtcod.so")
  (setf tcod:*root* (null-pointer)))

(defun close-libraries ()
  (cffi:close-foreign-library 'libtcod))

(defun game-loop ()
  (do* ((*game-running* t)
	(terrain-updated t)
	(fov-updated t)
	(player-last-xy nil)
	(tcod-black (tcod:compose-colour 0 0 0))
	(tcod-white (tcod:compose-colour 255 255 255)))
       ((not *game-running*))
    (debug-print 2000 "Looping.~%")
    (tcod:console-set-default-background tcod:*root* tcod-black)
    (tcod:console-clear tcod:*root*)
    (unless (null *game-player*)
      (let ((map-width (level-width *game-current-level*))
	    (map-height (level-height *game-current-level*))
	    (tiles (level-tiles *game-current-level*)))
	(unless (eq (creature-xy *game-player*) player-last-xy)
	  (setf fov-updated t)
	  (setf player-last-xy (creature-xy *game-player*)))
	(unless (not fov-updated)
	  (let ((fov-map (level-acquire-obstacle-map *game-current-level*)))
	    (debug-print 50 "Updating FOV and lighting.~%")
	    (clear-lighting)
	    (add-light-from-source *game-torch* fov-map)
	    (dolist (brazier *game-braziers*)
	      (add-light-from-source brazier fov-map))
	    (tcod:map-compute-fov fov-map
				  (car (creature-xy *game-player*))
				  (cdr (creature-xy *game-player*))
				  (+ map-width map-height)
				  t
				  :fov-shadow)
	    (dotimes (x map-width)
	      (dotimes (y map-height)
		(let ((tile (tile-at *game-current-level* x y)))
		  (debug-print 2000 "Lighting at ~a ~a is ~a.~%" x y (tile-lighting tile))
		  (setf (tile-visible (aref tiles x y))
			(and 
			 (not (tile-dark tile))
			 (tcod:map-is-in-fov? fov-map x y))))))
	    (setf fov-updated nil)
	    (level-release-obstacle-map *game-current-level* fov-map)))
	(dotimes (x map-width)
	  (dotimes (y map-height)
	    (let* ((tile (aref tiles x y))
		   (appearance (appearance-at tile))
		   (bg-light (coerce (min 1 (tile-lighting tile)) 'float))
		   (fg-light (coerce (min 1 (tile-lighting tile)) 'float))
		   (visible (tile-visible tile)))
	      (unless (null (tile-creature tile))
		(setf fg-light (coerce (max fg-light +minimum-fg-light+) 'float)))
	      (unless (not (and (= x (player-x))
				(= y (player-y))))
		(setf visible t))
	      (unless (not (and (<= (abs (- x (player-x))) 1)
				(<= (abs (- y (player-y))) 1)
				(not (tile-walkable tile))))
		(setf fg-light (coerce (max fg-light +minimum-touched-light+) 'float))
		(setf bg-light (coerce (max bg-light +minimum-touched-light+) 'float))
		(setf visible t))
	      (unless (not *cheat-lightall*)
		(setf visible t
		      bg-light 1.0
		      fg-light 1.0))
	      (unless (not visible)
		(tcod:console-put-char-ex tcod:*root*
					  x
					  (+ y +ui-top-lines+)
					  (appearance-glyph appearance)
					  (tcod:color-multiply-scalar
					   (apply #'tcod:compose-colour
						  (appearance-foreground-colour appearance))
					   fg-light)
					  (tcod:color-multiply-scalar
					   (apply #'tcod:compose-colour
						  (appearance-background-colour appearance))
					   bg-light))))))))

    (debug-print 50 "Printing game-text-buffer: ~a.~%" *game-text-buffer*)
    
    (let ((y-offset 0))
      (tcod:console-set-default-background tcod:*root*
					   (tcod:compose-colour 0 0 0))
      (tcod:console-set-default-foreground tcod:*root*
					   (tcod:compose-colour 0 255 255))
      (dolist (entry-with-count (reverse *game-text-buffer*))
	(let ((entry (if (<= (cdr entry-with-count) 1)
			 (car entry-with-count)
			 (format nil "~a [x~a]" (car entry-with-count) (cdr entry-with-count)))))
	  (unless (> y-offset +ui-top-lines+)
	    (incf y-offset (tcod:console-print-rect-ex tcod:*root*
						       0
						       y-offset
						       +screen-width+
						       (- +ui-top-lines+ y-offset)
						       :set
						       :left
						       entry))))))
    (unless (not *buffer-clear-time*)
      (debug-print 50 "Buffer wrap up time~%")
      (buffer-clear))
    (tcod:console-set-default-background tcod:*root*
					 (tcod:compose-colour 0 0 0))
    (tcod:console-set-default-foreground tcod:*root*
					 (tcod:compose-colour 255 255 255))
    (tcod:console-print-ex tcod:*root*
			   0
			   (- +screen-height+ +ui-bottom-lines+)
			   :set
			   :left
			   (make-statusline))
    (tcod:console-print-ex tcod:*root*
			   0
			   (+ 1 (- +screen-height+ +ui-bottom-lines+))
			   :set
			   :left
			   (make-statusline-2))
    
    (tcod:console-flush)
    
    (debug-print 100 "Handling input.~%")

    (debug-print 100 "Before input, game-text-buffer is: ~a.~%" *game-text-buffer*)

    (let* ((key (tcod:console-wait-for-keypress t)))
      (handle-keyboard-input key))

    (debug-print 100 "Game-text-buffer is now: ~a.~%" *game-text-buffer*))

  (terminate))

(defun make-statusline ()
  (if (null *game-player*)
      ""
      (format nil "~a [~a/~a]"
	      (indefinite-noun (creature-name *game-player*))
	      (creature-hp *game-player*)
	      (creature-max-hp *game-player*))))

(defun make-statusline-2 ()
  (if (null *game-player*)
      ""
      (let ((statuses nil))
	(unless (not (tile-dark (creature-tile *game-player*)))
	  (push "Darkness " statuses))
	(apply #'concatenate (cons 'string statuses)))))
    
	  

(defun quit-game ()
    (setf *game-running* nil))

(defun handle-input (thing stack)
  (debug-print 40 "Handling input ~a with hook stack: ~a~%" thing stack)
  (if (null stack)
      (debug-print 100 "Unhooked input: ~a.~%" thing)
      (funcall (car stack) thing (cdr stack))))

(defun handle-keyboard-input (key)
  (debug-print 200 "Handling input: ~a.~%" key)
  (let ((vk (tcod:key-vk key)))
    (cond
      ((eq vk :CHAR) (handle-input (tcod:key-c key) *game-input-hooks-stack*))
      (t (handle-input (tcod:key-vk key) *game-input-hooks-stack*)))))

(defun move-command (dx dy)
  ;; Later, we might not be moving the player, but, say, a cursor.
  (try-move-player dx dy))

(defmacro toggle (variable)
  `(setf ,variable (not ,variable)))

(defun make-standard-input-hooks ()
  #'(lambda (value stack)
	    (case value
	      (#\h (move-command -1 0))
	      (#\j (move-command 0 1))
	      (#\k (move-command 0 -1))
	      (#\l (move-command 1 0))
	      (#\y (move-command -1 -1))
	      (#\u (move-command 1 -1))
	      (#\b (move-command -1 1))
	      (#\n (move-command 1 1))
	      (#\d (try-player-drop-stack))
	      (#\, (try-player-pick-up-stack))
	      (#\V (toggle *cheat-lightall*))
	      (#\X (cheat-spawn-thingy))
	      (#\C (cheat-spawn-doodad))
	      (#\Q (query-confirm "Really quit?" #'quit-game))
	      (t (handle-input value stack)))))

(defun start-game ()
  (let* ((game-title "Light7DRL"))
    (debug-print 50 "Running with logging level ~a.~%" *debug-level*)
    (load-libraries)
    (tcod:console-set-custom-font *tileset-file*
				  '(:font-layout-ascii-in-row)
				  16 16)
    (tcod:console-init-root +screen-width+
			    +screen-height+
			    game-title
			    nil ;; not a fullscreen game
			    :renderer-sdl)
    (tcod:console-set-keyboard-repeat 500 10)
    (setf *game-input-hooks-stack* nil)
    (push-hooks (make-standard-input-hooks))
    (unless *game-initialized*
      (initialize-first-game)
      (setf *game-initialized* t))
    (debug-print 100 "Entering loop.~%")
    (game-loop)
    (debug-print 100 "End loop.~%")))
  

(defun game-main ()
  (start-game)
  (close-libraries)
  (terminate))
