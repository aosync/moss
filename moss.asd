(defsystem "moss"
  :description "Optimizing compilation playground"
  :author "Alejandro W. Sior <aho@sior.be>"
  :version "0.0.0"
  :serial t
  :components ((:file "package")
	       (:file "static")
	       (:file "basic-block")
	       (:file "interactive")))
