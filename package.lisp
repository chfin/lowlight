;;;; package.lisp

(defpackage #:lowlight.1
  (:use #:cl)
  (:nicknames #:lowlight #:ll #:ll1)
  (:export #:light #:light-file
	  #:light-blocks #:light-file-blocks
	  #:define-cfg-style #:make-cfg-style
	  #:define-simple-style #:make-simple-style
	  #:style-classes
	  #:*lexer-buffer-size* #:*debug*))
