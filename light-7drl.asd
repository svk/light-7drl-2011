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
		 (:file "creature"
			:depends-on ("rolls" "world-structs" "grammar" "package" "globals"))
		 (:file "world"
			:depends-on ("rolls" "world-structs" "grammar" "creature" "package" "globals"))
		 (:file "main"
			:depends-on ("world-structs" "grammar" "creature" "package" "globals" "world"))))
