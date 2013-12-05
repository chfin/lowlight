;;;; lowlight.asd

(asdf:defsystem #:lowlight
  :serial t
  :description "A simple and flexible syntax highlighter"
  :author "Christoph Finkensiep <chfin@freenet.de>"
  :license "MIT/X11"
  :version "0.99.0"
  :depends-on (#:cl-ppcre #:alexandria #:spinneret #:cl-who #:graylex #:yacc)
  :components ((:file "package")
	       (:file "common")
	       (:file "simple")
	       (:file "grammar")
	       (:file "lowlight")
	       (:file "style-lisp")))
