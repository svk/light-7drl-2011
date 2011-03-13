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
	(fov-updated t)
	(player-last-xy nil)
	(tcod-black (tcod:compose-colour 0 0 0))
	(tcod-white (tcod:compose-colour 255 255 255)))
       ((not *game-running*))
    (debug-print 2000 "Looping.~%")
    (if (not (null *game-special-screen-stack*))
	(funcall (car *game-special-screen-stack*))
	(progn
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
		  (debug-print 50 "Updating FOV.~%")
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
		    (let ((fg (tcod:color-multiply-scalar
			       (apply #'tcod:compose-colour
				      (appearance-foreground-colour appearance))
			       fg-light))
			  (bg (tcod:color-multiply-scalar
			       (apply #'tcod:compose-colour
				      (appearance-background-colour appearance))
			       bg-light)))
		      (unless (not (look-mode-highlighted? x y))
			(debug-print 50 "Highlighting! ~a ~a~%" x y)
			(setf fg (tcod:compose-colour 0 0 0))
			(setf bg (tcod:compose-colour 255 0 255)))
		      (cond
			(visible
			 (tcod:console-put-char-ex tcod:*root*
						   x
						   (+ y +ui-top-lines+)
						   (if (and (look-mode-highlighted? x y)
							    (eq (appearance-glyph appearance)
								+wall-glyph+))
						       (char-code #\ )
						       (appearance-glyph appearance))
						   fg
						   bg))
			((look-mode-highlighted? x y)
			 (tcod:console-put-char-ex tcod:*root*
						   x
						   (+ y +ui-top-lines+)
						   (char-code #\ )
						   bg
						   bg)))))))))

	  (debug-print 50 "Printing game-text-buffer: ~a.~%" *game-text-buffer*)
	  
	  (let ((y-offset 0))
	    (tcod:console-set-default-background tcod:*root*
						 (tcod:compose-colour 0 0 0))
	    (tcod:console-set-default-foreground tcod:*root*
						 (tcod:compose-colour 0 255 255))
	    (block buffer-print-loop
	      (do* ((entries (reverse *game-text-buffer*) (cdr entries))
		    (entry-with-count (car entries) (car entries)))
		   ((null entries))
		(let ((entry (if (<= (cdr entry-with-count) 1)
				 (car entry-with-count)
				 (format nil "~a [x~a]" (car entry-with-count) (cdr entry-with-count)))))
		  (if (< y-offset +ui-top-lines+)
		      (incf y-offset (tcod:console-print-rect-ex tcod:*root*
								 0
								 y-offset
								 +screen-width+
								 (- +ui-top-lines+ y-offset)
								 :set
								 :left
								 entry))
		      (return-from buffer-print-loop
			(progn
			  (tcod:console-print-rect-ex tcod:*root*
						      0
						      y-offset
						      +screen-width+
						      1
						      :set
						      :left
						      "[More]")
			  (setf *buffer-clear-time* nil)
			  (query-space-or-enter #'(lambda ()
						    (setf *game-text-buffer* (reverse entries)))))))))))
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
	  (tcod:console-print-rect-ex tcod:*root*
				      0
				      (+ 2 (- +screen-height+ +ui-bottom-lines+))
				      +screen-width+
				      2
				      :set
				      :left
				      (make-inventory-lines))))
    (tcod:console-flush)
	  
    (debug-print 100 "Handling input.~%")

    (debug-print 100 "Before input, game-text-buffer is: ~a.~%" *game-text-buffer*)

    (let* ((key nil))
      (ignore-errors  ;; pressing international chars causes cl-tcod to break
	(setf key (tcod:console-wait-for-keypress t)))
      (unless (null key)
	(handle-keyboard-input key)))

    (debug-print 100 "Game-text-buffer is now: ~a.~%" *game-text-buffer*))

  (terminate))

(defun format-item (item)
  (let ((suffixes nil)
	(main (format nil "~a: ~a" (item-inventory-slot item) (indefinite-noun (item-name item )))))
    (if (eq (creature-weapon *game-player*) item)
	(push "wielded" suffixes))
    (if (not (null (item-ammo item)))
	(push (format nil "~a" (item-ammo item)) suffixes))
    (if suffixes
	(format nil "~a (~a)" main (comma-join suffixes))
	main)))

(defun make-inventory-lines ()
  (if (null *game-player*)
      ""
      (list-join
       (mapcar #'format-item
	       (remove-if-not #'identity
			      (mapcar #'(lambda (i-slot) (creature-get-item-by-slot *game-player* i-slot))
				      *inventory-chars*))))))

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
	(unless (not (get-status *game-player* :poison))
	  (push "Poisoned " statuses))
	(unless (not (creature-is-burdened? *game-player*))
	  (push "Burdened " statuses))
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
      (let ((movement-xy (translate-movement-input value)))
	(if movement-xy
	    (move-command (car movement-xy) (cdr movement-xy))
	    (case value
	      (#\. (player-wait))
	      (#\a (try-use-special))
	      (#\d (try-player-drop-query))
	      (#\x (try-player-apply-query))
	      (#\, (try-player-pick-up-stack))
	      (#\: (enter-look-mode (object-level *game-player*) (player-x) (player-y)))
	      (#\Q (query-confirm "Really quit?" #'quit-game))

	      (#\W (go-to-next-level))
	      (#\A (show-game-ending))
	      (#\V (toggle *cheat-lightall*))

	      (t (handle-input value stack)))))))

(defun start-game ()
  (let* ((game-title "Light7DRL"))
    (uninitialize-game)
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
    (block main-game
      (handler-bind
	  ((game-over #'(lambda (condition)
			  (with-slots (type)
			      condition
			    (debug-print 1 "Game over: ~a.~%" type))
			  (query-space-or-enter
			   #'(lambda ()
			       (push-special-screen
				(with-slots (type)
				    condition
				  (cond ((eq type :death)
					 (make-game-over-death-screen condition))
					((eq type :victory)
					 (make-game-over-victory-screen condition))
					(t (make-centered-text-special-screen "Game over! ..wait, what?")))))
			       (query-space-or-enter
				#'(lambda ()
				    (pop-special-screen)
				    (uninitialize-game)
				    (buffer-clear)
				    (query-confirm
				     "Play again?"
				     #'(lambda ()
					 (initialize-first-game))
				     #'(lambda ()
					 (return-from main-game))))))))))
	(game-loop)))
    (debug-print 100 "End loop.~%")))
  

(defun game-main ()
  (start-game)
  (close-libraries)
  (terminate))
