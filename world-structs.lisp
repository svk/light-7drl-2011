(in-package :light-7drl)

(defstruct appearance
  glyph
  (foreground-colour '(255 255 255))
  (background-colour nil))

(defstruct tile
  appearance
  opaque
  walkable
  (visible nil)
  (creature nil)
  (items nil)
  (lighting 0)
  (explored nil)
  (special nil))

(defstruct level
  width
  height
  tiles
  (creatures nil)
  (floor-items nil)
  (obstacle-map nil)
  (obstacle-map-updated nil)
  (unexplored nil)
  (delayed-spawns nil))

(defstruct light-source
  x
  y
  intensity
  (cached-fov nil))

(defclass radiant ()
  ((light-source :initform nil
		 :initarg :light-source)
   (level  :accessor object-level
	   :initform nil
	   :initarg :level)))


(defclass item (radiant)
  ((appearance :accessor item-appearance
	       :initarg :appearance)
   (name :accessor item-name
	 :initarg :name)
   (heavy :initarg :heavy
	  :initform nil)
   (type :accessor item-type
	 :initarg :type)
   (inventory-slot :accessor item-inventory-slot
		   :initform nil)
   (active :accessor item-active
	   :initarg :active
	   :initform nil)
   (attack-power :accessor item-attack-power
		 :initform nil
		 :initarg :attack-power)
   (ammo :accessor item-ammo
	 :initform nil
	 :initarg :ammo)))

(defclass creature (radiant)
  ((appearance :accessor creature-appearance
	       :initarg :appearance)
   (name :accessor creature-name
	 :initarg :name)
   (gender :accessor creature-gender
	   :initform nil
	   :initarg :gender)
   (hp :accessor creature-hp
       :initarg :hp)
   (max-hp :accessor creature-max-hp
	   :initarg :max-hp)
   (ai :accessor creature-ai
       :initform nil
       :initarg :ai)
   (tile :accessor creature-tile
	 :initform nil
	 :initarg :tile)
   (xy :accessor creature-xy
       :initform nil
       :initarg :xy)
   (items :accessor creature-items
	  :initform nil
	  :initarg :items)
   (stepmap-to :initform nil)
   (fov :accessor creature-fov
	:initform nil)
   (darkvision :accessor creature-darkvision
	       :initform nil
	       :initarg :darkvision)
   (base-hit-chance :initarg :hit-chance)
   (base-damage :initarg :damage)
   (attack-verb :initarg :attack-verb)
   (hit-verb :initarg :hit-verb)
   (miss-verb :initarg :miss-verb)
   (attack-inflicts-status :initarg :attack-inflicts-status
			   :initform nil)
   (speed :initarg :speed
	  :initform 1)
   (hooks :initform (make-hash-table))
   (dodge-multiplier :initform 1
		     :initarg :dodge-multiplier)
   (statuses :initform nil)
   (weapon 
    :accessor creature-weapon
    :initform nil)))

(defstruct status-attack
  type
  verb)
  
