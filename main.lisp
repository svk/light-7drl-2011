(require :asdf)
(asdf:operate 'asdf:load-op :cffi)
(asdf:operate 'asdf:load-op :tcod)

(defpackage :light-7drl
  (:use :common-lisp :cffi :tcod))
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

(defconstant +wall-glyph+ 219)
(defconstant +player-glyph+ (char-code #\@))
(defconstant +floor-glyph+ (char-code #\.))

(defstruct appearance
  glyph
  (foreground-colour '(255 255 255))
  (background-colour '(0 0 0)))

(defstruct tile
  appearance
  opaque
  walkable
  (visible nil)
  (creature nil)
  (items nil))

(defstruct creature
  appearance
  (xy nil))

(defun creature-can-walk? (creature tile)
  (and (tile-walkable tile)
       (not (tile-creature tile))))

(defun spawn-creature (creature map)
  (let ((xy (select-random 
	     (remove-if-not #'(lambda (xy) (creature-can-walk? creature (aref map (car xy) (cdr xy))))
			    (find-walkables map)))))
    (setf (creature-xy creature) xy)
    (setf (tile-creature (aref map (car xy) (cdr xy))) creature)))

(defun make-wall-tile ()
  (make-tile :appearance (make-appearance :glyph +wall-glyph+
					  :foreground-colour '(0 0 255)
					  :background-colour '(0 0 0))
	     :opaque t
	     :walkable nil))

(defun make-floor-tile ()
  (make-tile :appearance (make-appearance :glyph +floor-glyph+
					  :foreground-colour '(255 255 255)
					  :background-colour '(0 0 0))
	     :opaque nil
	     :walkable t))

(defun appearance-at (tile)
  (unless (not (tile-creature tile))
    (return-from appearance-at (creature-appearance (tile-creature tile))))
;;  (unless (not (tile-items tile))
;;    (return-from appearance-at (item-appearance (car (tile-items tile)))))
  (tile-appearance tile))

(defun create-game-map (width height)
  (let ((rv (make-array (list width height))))
    (dotimes (x width)
      (dotimes (y height)
	(setf (aref rv x y)
	      (if (or (= x 0)
		      (= y 0)
		      (= (+ 1 x) width)
		      (= (+ 1 y) height))
		  (make-wall-tile)
		  (make-floor-tile)))))
    rv))

(defun find-walkables (map)
  (do* ((width (car (array-dimensions map)))
	(height (cadr (array-dimensions map)))
	(x 0 (+ x 1))
	(rv nil))
       ((>= x width) rv)
    (dotimes (y height)
      (unless (not (tile-walkable (aref map x y)))
	(push (cons x y) rv)))))

(defun select-random (list)
  (nth (random (length list)) list))

(defun find-random-walkable (map)
  (select-random (find-walkables map)))

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
  
(defun game-main (screen-width
		  screen-height
		  font-filename)
  (let* ((keep-running t)
	 (game-title "Light7DRL")
	 (top-ui-lines 3)
	 (bottom-ui-lines 2)
	 (map-width screen-width)
	 (map-height (- screen-height top-ui-lines bottom-ui-lines))
	 (game-map (create-game-map map-width map-height))
	 (text-buffer nil)
	 (player (spawn-creature 
		  (make-creature
		   :appearance (make-appearance :glyph +player-glyph+
						:foreground-colour '(255 0 0)
						:background-colour '(0 0 0)))
		  game-map)))
    (push "Welcome to Light7DRL!" text-buffer)
    (push "For now, darkness awaits." text-buffer)
    (load-libraries)
    (tcod:console-set-custom-font font-filename
				  '(:font-layout-ascii-in-row)
				  16 16)
    (tcod:console-init-root screen-width
			    screen-height
			    game-title
			    nil ;; not a fullscreen game
			    :renderer-sdl)
    (tcod:console-set-keyboard-repeat 500 100)
    (format t "Entering loop~%")
    (do ((fov-map (tcod:map-new map-width map-height))
	 (terrain-updated t)
	 (fov-updated t)
	 (tcod-black (tcod:compose-colour 0 0 0))
	 (tcod-white (tcod:compose-colour 255 255 255)))
	((not keep-running))
      (format t "Looping~%")
      (unless (not terrain-updated)
	(set-tcod-opacity-map game-map fov-map)
	(setf terrain-updated nil))
      (unless (not fov-updated)
	(tcod:map-compute-fov fov-map
			      (car (creature-xy player))
			      (cdr (creature-xy player))
			      (+ map-width map-height)
			      t
			      :fov-shadow)
	(dotimes (x map-width)
	  (dotimes (y map-height)
	    (setf (tile-visible (aref game-map x y))
		  (tcod:map-is-in-fov? fov-map x y))))
	(setf fov-updated t))
      
      (tcod:console-set-default-background tcod:*root* tcod-black)
      (tcod:console-clear tcod:*root*)
      (dotimes (x map-width)
	(dotimes (y map-height)
	  (let* ((tile (aref game-map x y))
		 (appearance (appearance-at tile)))
	    (unless (not (tile-visible tile))
	      (tcod:console-put-char-ex tcod:*root*
					x
					(+ y top-ui-lines)
					(appearance-glyph appearance)
					(apply #'tcod:compose-colour
					       (appearance-foreground-colour appearance))
					(apply #'tcod:compose-colour
					       (appearance-background-colour appearance)))))))
      (tcod:console-flush)

      (let* ((key (tcod:console-wait-for-keypress t))
	     (vk (tcod:key-vk key)))
	(cond
	  ((eq vk :CHAR)
	   (case (tcod:key-c key)
	     (#\q (setf keep-running nil)))))))
    (format t "End loop~%")
    (close-libraries)
    (terminate)))
