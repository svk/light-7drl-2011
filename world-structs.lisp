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
  (lighting 0))

(defstruct item
  appearance
  name
  size)

(defstruct level
  width
  height
  tiles
  (creatures nil)
  (obstacle-map nil)
  (obstacle-map-updated nil))

(defstruct light-source
  x
  y
  intensity
  (cached-fov nil))

(defclass creature ()
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
   (level :accessor creature-level
	  :initform nil
	  :initarg :level)
   (fov :accessor creature-level
	:initform nil)))