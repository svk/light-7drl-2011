(in-package :light-7drl)

(defun make-item (&rest rest)
  (apply #'make-instance
	 (cons 'item
	       rest)))

(defun make-weapon (&rest rest)
  (apply #'make-item rest))

(defmethod item-heavy? ((item item))
  (with-slots (heavy)
      item
    heavy))

(defmethod consume-item ((creature creature) (item item))
  (setf (creature-items creature)
	(remove item (creature-items creature))))

(defmethod player-use ((item item))
  ;; Why is this a huge switch statement? Because I have only a little over 12h left
  ;; and no sleep
  (with-slots (type inventory-slot name active)
      item
    (case type
      (:melee-weapon
       (setf (creature-weapon *game-player*)
	     item)
       (buffer-show "You wield ~a.~%" (definite-noun name))
       (player-took-action))
      (:torch
       (if active
	   ;; Anyone can snuff out a torch without equipment
	   (progn
	     (buffer-show "You snuff out the flame.")
	     (toggle active)
	     (player-took-action))
	   (progn
	     (if (not (consume-fire-light-charge))
		 (buffer-show "You don't have anything with which to ignite a flame.")
		 (progn
		   (buffer-show "You light the torch.")
		   (toggle active)
		   (player-took-action))))))
      (:brazier
       (if active
	   (progn
	     (if (not (consume-fire-snuff-charge))
		 (buffer-show "You don't have anything with which to snuff the flame.")
		 (progn
		   (buffer-show "You snuff out the flame.")
		   (toggle active)
		   (player-took-action))))
	   (progn
	     (if (not (consume-fire-light-charge))
		 (buffer-show "You don't have anything with which to ignite a flame.")
		 (progn
		   (buffer-show "You set the brazier aflame.")
		   (toggle active)
		   (player-took-action))))))
      (:healing-potion
       (if (= (creature-hp *game-player*)
	      (creature-max-hp *game-player*))
	   (buffer-show "You drink ~a. Nothing happens." (definite-noun name))
	   (buffer-show "You drink ~a. You feel rejuvenated." (definite-noun name)))
       (setf (creature-hp *game-player*)
	     (creature-max-hp *game-player*))
       (consume-item *game-player* item)
       (player-took-action))
      (:antidote-potion
       (if (not (get-status *game-player* :poison))
	   (buffer-show "You drink ~a. Nothing happens." (definite-noun name))
	   (buffer-show "You drink ~a. You feel better." (definite-noun name)))
       (consume-item *game-player* item)
       (player-took-action))
      (t (buffer-show "You're not sure how you could make use of ~a right now."
		      (definite-noun name))))))

(defmethod creature-is-burdened? ((creature creature))
  (with-slots (items)
      creature
    (not (null (find-if #'item-heavy? items)))))

(defmethod creature-get-free-inventory-slot ((creature creature))
  (dolist (inv-slot *inventory-chars*)
    (let ((rv (creature-has-inventory-slot-free? creature inv-slot)))
      (if rv
	  (return-from creature-get-free-inventory-slot rv)))))

(defmethod creature-get-item-by-slot ((creature creature) inv-slot)
  (dolist (item (creature-items creature))
    (unless (not (equal inv-slot (item-inventory-slot item)))
      (return-from creature-get-item-by-slot item))))

(defmethod creature-has-inventory-slot-free? ((creature creature) inv-slot)
  (unless (null inv-slot)
    (if (creature-get-item-by-slot creature inv-slot)
	nil
	inv-slot)))

(defmethod creature-has-inventory-room-for? ((creature creature) (item item))
  (with-slots (items)
      creature
    (and (<= (+ 1 (length items)) (length *inventory-chars*))
	 (or (not (item-heavy? item))
	     (not (creature-is-burdened? creature))))))

(defun spawn-item-for (creature generator)
  (let ((item (funcall generator)))
    (if (creature-has-inventory-room-for? creature item)
	(creature-give creature item)
	(drop-at item (object-level creature) (creature-x creature) (creature-y creature)))))
    

(defmethod emit-light ((item item))
  (with-slots (light-source level active)
      item
    (unless (or (not active)
		(null light-source)
		(null level))
      (debug-print 50 "Item ~a is emitting light.~%" item)
      (let ((fov-map (level-acquire-obstacle-map level)))
	(add-light-from-source light-source fov-map)
	(level-release-obstacle-map level fov-map)))))


(defmethod drop-at ((item item) new-level x y)
  (with-slots (level light-source)
      item
    (setf level new-level)
    (unless (null light-source)
      (move-light-source light-source x y))
    (push item (level-floor-items level))
    (push item (tile-items (tile-at level x y)))))

(defmethod picked-up ((item item))
  (with-slots (level)
      item
    (unless (null level)
      (setf (level-floor-items level)
	    (remove item (level-floor-items level))))
    (setf level nil)))

(defmethod item-given ((item item) (creature creature))
  (setf (item-inventory-slot item)
	(or (creature-has-inventory-slot-free? creature (item-inventory-slot item))
	    (creature-get-free-inventory-slot creature))))

(defmethod radiant-intensity ((radiant radiant))
  (with-slots (light-source active)
      radiant
    (if (or (not active)
	    (null light-source))
	0
	(light-source-intensity light-source))))

(defmethod tick ((item item))
  (with-slots (active light-source ammo)
      item
    (unless (not (and active light-source ammo))
	(decf ammo)
	(setf ammo (max 0 ammo))
	(if (<= ammo 0)
	    (setf active nil)
	    (setf (light-source-intensity light-source)
		  (* +torch-max-intensity+
		     (/ ammo +torch-ammo+)))))))
	
