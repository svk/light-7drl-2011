(asdf:defsystem #:light-7drl
    :depends-on (#:cffi #:tcod)
    :components ((:file "package")
		 (:file "globals"
			:depends-on ("package"))
		 (:file "world"
			:depends-on ("package" "globals"))
		 (:file "main"
			:depends-on ("package" "globals" "world"))))
