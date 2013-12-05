;;;; style-c.lisp

(in-package #:lowlight.0)

(defparameter *c-keywords*
  '("auto" "break" "case" "char" "const"
    "continue" "default" "do" "double" "else"
    "enum" "extern" "float" "for" "goto"
    "if" "int" "long" "register" "return"
    "short" "signed" "sizeof" "static" "struct"
    "switch" "typedef" "union" "unsigned" "void"
    "volatile" "while" "__restrict" "_Bool"))

(let ((com1 "(//.*?[\\n\\r]{1,2})")
      (com2 "(//.*?$)")
      (com3 "(/\\*.*?\\*/)")
      (str1 "(\".*?[^\\\\]\")")
      (str2 "(\"\")")
      (char "('\\\\?.'])"))
  
  (define-style :c
    
    (("~{~a~^|~}" (list com1 com2 com3 str1 str2 char))
     1 (case (char ($ 1) 0)
	 (#\/ (cons "comment" ($ 1)))
	 (#\" (cons "string" ($ 1)))
	 (#\' (cons "character" ($ 1)))
	 (t (error "Could not parse ~a" ($ 1)))))
    (("~{~a~^|~}" *c-keywords*)
     1 (cons "keyword" ($ 1)))))
