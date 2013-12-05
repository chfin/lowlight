;;;; tests/lowlight.tests.asd

(asdf:defsystem #:lowlight.tests
  :serial t
  :description "Tests for lowlight"
  :author "Christoph Finkensiep <chfin@freenet.de>"
  :license "X11/MIT"
  :depends-on (#:lowlight
               #:fiveam)
  :components ((:file "package")
               (:file "lowlight.tests")))
