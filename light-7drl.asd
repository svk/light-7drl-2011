(asdf:defsystem #:light-7drl
    :depends-on (#:cffi #:tcod)
    :components ((:file "package")
		 (:file "grammar"
			:depends-on ("package"))
		 (:file "globals"
			:depends-on ("grammar" "package"))
		 (:file "world"
			:depends-on ("grammar" "package" "globals"))
		 (:file "main"
			:depends-on ("grammar" "package" "globals" "world"))))
