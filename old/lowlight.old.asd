;;;; lowlight.asd

(asdf:defsystem #:lowlight.old
  :serial t
  :description "A simple and flexible syntax highlighter (old 0.x version)"
  :author "Christoph Finkensiep <chfin@freenet.de>"
  :license "MIT"
  :depends-on (#:cl-ppcre #:alexandria #:spinneret #:cl-who)
  :components ((:file "package")
               (:file "lowlight")
	       (:file "style-lisp")
	       (:file "style-c")))
