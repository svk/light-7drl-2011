(asdf:defsystem #:light-7drl
    :depends-on (#:cffi #:tcod)
    :components ((:file "package")
		 (:file "grammar"
			:depends-on ("package"))
		 (:file "globals"
			:depends-on ("grammar" "package"))
		 (:file "world-structs"
			:depends-on ("package"))
		 (:file "dungeon"
			:depends-on ("package"))
		 (:file "rolls"
			:depends-on ("package"))
		 (:file "visuals"
			:depends-on ("package"))
		 (:file "ai"
			:depends-on ("package" "rolls" "world-structs"))
		 (:file "creature"
			:depends-on ("ai" "rolls" "world-structs" "grammar" "package" "globals"))
		 (:file "monsters"
			:depends-on ("ai" "rolls" "creature"))
		 (:file "item"
			:depends-on ("package" "globals" "world-structs"))
		 (:file "world"
			:depends-on ("monsters" "item" "visuals" "ai" "rolls" "world-structs" "grammar" "creature" "package" "globals"))
		 (:file "main"
			:depends-on ("ai" "item" "world-structs" "grammar" "creature" "package" "globals" "world"))))
