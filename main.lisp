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

(defun set-tcod-opacity-map (map fov-map)
  (let ((width (car (array-dimensions map)))
	(height (cadr (array-dimensions map))))
    (dotimes (x width)
      (dotimes (y height)
	(tcod:map-set-properties fov-map
				 x
				 y
				 (not (tile-opaque (aref map x y)))
				 nil)))))

(defun game-loop ()
  (do* ((*game-running* t)
	(map-width (car (array-dimensions *game-map*)))
	(map-height (cadr (array-dimensions *game-map*)))
	(fov-map (tcod:map-new map-width map-height))
	(terrain-updated t)
	(fov-updated t)
	(player-last-xy nil)
	(tcod-black (tcod:compose-colour 0 0 0))
	(tcod-white (tcod:compose-colour 255 255 255)))
       ((not *game-running*))
    (debug-print 2000 "Looping.~%")
    (unless (eq (creature-xy *game-player*) player-last-xy)
      (setf fov-updated t)
      (setf player-last-xy (creature-xy *game-player*)))
    (unless (not terrain-updated)
      (debug-print 50 "Updating opacity map.~%")
      (set-tcod-opacity-map *game-map* fov-map)
      (setf terrain-updated nil))
    (unless (not fov-updated)
      (debug-print 50 "Updating FOV and lighting.~%")
      (clear-lighting)
      (add-light-from-source *game-torch* fov-map)
      (add-light-from-source *game-brazier* fov-map)
      (tcod:map-compute-fov fov-map
			    (car (creature-xy *game-player*))
			    (cdr (creature-xy *game-player*))
			    (+ map-width map-height)
			    t
			    :fov-shadow)
      (dotimes (x map-width)
	(dotimes (y map-height)
	  (let ((tile (tile-at *game-map* x y)))
	    (debug-print 2000 "Lighting at ~a ~a is ~a.~%" x y (tile-lighting tile))
	    (setf (tile-visible (aref *game-map* x y))
		  (and 
		   (>= (tile-lighting tile) +light-visibility-threshold+)
		   (tcod:map-is-in-fov? fov-map x y))))))
      (setf fov-updated nil))
    
    (tcod:console-set-default-background tcod:*root* tcod-black)
    (tcod:console-clear tcod:*root*)
    (dotimes (x map-width)
      (dotimes (y map-height)
	(let* ((tile (aref *game-map* x y))
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
				       bg-light))))))
    
    (let ((y-offset 0))
      (tcod:console-set-default-background tcod:*root*
					   (tcod:compose-colour 0 0 0))
      (tcod:console-set-default-foreground tcod:*root*
					   (tcod:compose-colour 0 255 255))
      (dolist (entry (reverse *game-text-buffer*))
	(incf y-offset (tcod:console-print-rect-ex tcod:*root*
						   0
						   y-offset
						   +screen-width+
						   (- +ui-top-lines+ y-offset)
						   :set
						   :left
						   entry))))
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
    (setf *game-text-buffer* nil)
    
    (tcod:console-flush)
    
    (let* ((key (tcod:console-wait-for-keypress t)))
      (handle-keyboard-input key)))
  (terminate))

(defun make-statusline ()
  (format nil "~a [~a/~a]"
	  (creature-name *game-player*)
	  (creature-hp *game-player*)
	  (creature-max-hp *game-player*)))
	  

(defun quit-game ()
    (setf *game-running* nil))

(defun handle-input (thing)
  (let ((result (assoc thing *game-input-hooks* :test #'equal)))
    (if result
	(funcall (cdr result))
	(debug-print 100 "Unhooked input: ~a.~%" thing))))

(defun handle-keyboard-input (key)
  (debug-print 200 "Handling input: ~a.~%" key)
  (let ((vk (tcod:key-vk key)))
    (cond
      ((eq vk :CHAR) (handle-input (tcod:key-c key)))
      (t (handle-input (tcod:key-vk key))))))

(defun add-input-hook (thing hook)
  (push (cons thing hook) *game-input-hooks*))

(defun move-command (dx dy)
  ;; Later, we might not be moving the player, but, say, a cursor.
  (try-move-player dx dy))

(defun reset-standard-input-hooks ()
  (setf *game-input-hooks* nil)
  (add-input-hook #\h #'(lambda () (move-command -1 0)))
  (add-input-hook #\l #'(lambda () (move-command 1 0)))
  (add-input-hook #\j #'(lambda () (move-command 0 1)))
  (add-input-hook #\k #'(lambda () (move-command 0 -1)))
  (add-input-hook #\y #'(lambda () (move-command -1 -1)))
  (add-input-hook #\u #'(lambda () (move-command 1 -1)))
  (add-input-hook #\b #'(lambda () (move-command -1 1)))
  (add-input-hook #\n #'(lambda () (move-command 1 1)))
  (add-input-hook #\V #'(lambda () (setf *cheat-lightall* (not *cheat-lightall*))))
  (add-input-hook #\Q #'quit-game))

(defun start-game ()
  (let* ((game-title "Light7DRL"))
    (debug-print 50 "Running with logging level ~a.~%" *debug-level*)
    (load-libraries)
    (unless *game-initialized*
      (initialize-first-game)
      (setf *game-initialized* t))
    (tcod:console-set-custom-font *tileset-file*
				  '(:font-layout-ascii-in-row)
				  16 16)
    (tcod:console-init-root +screen-width+
			    +screen-height+
			    game-title
			    nil ;; not a fullscreen game
			    :renderer-sdl)
    (tcod:console-set-keyboard-repeat 500 10)
    (reset-standard-input-hooks)
    (debug-print 100 "Entering loop.~%")
    (game-loop)
    (debug-print 100 "End loop.~%")))
  

(defun game-main ()
  (start-game)
  (close-libraries)
  (terminate))
