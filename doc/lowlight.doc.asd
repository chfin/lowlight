;;;; lowlight.doc.asd

(asdf:defsystem #:lowlight.doc
  :serial t
  :description "Documentation generation for lowlight"
  :author "Christoph Finkensiep <chfin@freenet.de>"
  :license "MIT"
  :depends-on (#:lowlight #:cl-gendoc)
  :components ((:file "package")
               (:file "lowlight.doc")))
