;;;; style-lisp.lisp

(in-package #:lowlight)

(define-style :colon
  ("(:)" 1 (cons "colon" ($ 1))))

;;; SLIME-like common lisp style
;;; classes:
;;;   comment: a ';' comment section
;;;   quote: ' ` , #'
;;;   string: a "string literal"
;;;   keyword: a :keyword
;;;   number: a number literal, like 1 or #16r0
;;;   character: a character literal, like #\" or #\Newline
;;;   define: a symbol starting with def, like defun
;;;   defname: the first symbol or (setf bla) after a define
;;;   vardefname: the name in a defvar/defparameter form
;;;   typedefname: a type/class/struct name in the definition form
;;;   lambda: a lambda list keyword, like &key or &body
;;;   global: a *symbol* in *s
;;;   constant: a +symbol+ in +s
;;;   withmacro: a symbol starting with with, like with-slots
;;;   specialop: a special operator, like let
;;;   stdmacro: a macro from the CL package, like when
;;;   stdfun: a function from the CL package, like +
;;;   symbol: any other symbol

(defparameter *symbol-chars* "[^\\s'`,\"\\(\\);]")
(defparameter *symbol-starts* "[^\\s'`,\"\\(\\);:#]")
(defparameter *special-operators*
  '(quote if progn let let* setq
    flet labels macrolet symbol-macrolet function
    block return-from tagbody go catch throw unwind-protect
    multiple-value-call values eval-when locally the load-time-value progv))

;;I won't support |stuff|-like-|that| !!!
(let ((com1 "(;.*?[\\n\\r]{1,2})")
      (com2 "(;.*?$)")
      (com3 "(#\\|.*?\\|#)")
      (str1 "(\".*?[^\\\\]\")")
      (str2 "(\"\")")
      (qchar "(#\\\\[\\\";])")
      (keyw (format nil "(:~a*?\\|.*?\\|~a*)"
		    *symbol-chars* *symbol-chars*))
      (symb1 (format nil "(~a~a*?\\|.*?\\|~a*)"
		     *symbol-starts* *symbol-chars* *symbol-chars*))
      (symb2 (format nil "(\\|.*?\\|~a*)" *symbol-chars*))
      (defs (format nil
		    "(def)(un|macro|method|generic|setf|ine-~a+)"
		    *symbol-chars*))
      (+- "[\\+\\-]?"))

  (define-style :common-lisp

    ;;special cases
    
    (("~{~a~^|~}" (list com1 com2 com3 str1 str2 qchar keyw symb1 symb2))
     1 (case (char ($ 1) 0)
	 (#\; (cons "comment" ($ 1)))
	 (#\" (cons "string" ($ 1)))
	 (#\# (cons "character" ($ 1)))
	 (#\: (cons "keyword" ($ 1)))
	 (t (cons "symbol" ($ 1)))))
    
    ;;literals

    ("(#'|[,'`]+)" 1 (cons "quote" ($ 1)))

    (("([\\s'`,\"\\(\\);])(:~a+)" *symbol-chars*)
     2 ($ 1) (cons "keyword" ($ 2)))
    
    ("(#\\\\)(space|newline|tab|page|return|linefeed|\\w)"
     2 (cons "character" (concatenate 'string ($ 1) ($ 2))))
    
    (("(~a\\d[\\d\\./e]*)|(#[box]~a[\\da-f/]*)|(#\\d*r~a[\\d\\w/])" +- +- +-)
     1 (cons "number" ($ 1)))
    
    ;;definitions
    
    (("~a(\\s*\\(?\\s*)(setf\\s*~a~a*|~a~a*)"
      defs *symbol-starts* *symbol-chars* *symbol-starts* *symbol-chars*)
     4 ($ 1) ($ 2) ($ 3) (cons "defname" ($ 4)))
    
    (("(def)(class|struct|type)(\\s*)(~a~a*)" *symbol-starts* *symbol-chars*)
     4 ($ 1) ($ 2) ($ 3) (cons "typedefname" ($ 4)))

    (("(def)(var|parameter)(\\s*)(~a~a*)" *symbol-starts* *symbol-chars*)
     4 ($ 1) ($ 2) ($ 3) (cons "vardefname" ($ 4)))

    (("(def~a*)" *symbol-chars*)
     1 (cons "define" ($ 1)))

    ;;special symbol forms
    
    ("(&key|&optional|&body|&rest|&whole|&allow-other-keys|&aux|&environment)"
     1 (cons "lambda" ($ 1)))

    (("(\\*~a*\\*)" *symbol-chars*)
     1 (cons "global" ($ 1)))
    
    (("(\\+~a*\\+)" *symbol-chars*)
     1 (cons "constant" ($ 1)))

    ;; special operators and functions

    (("(with-~a*)" *symbol-starts* *symbol-chars* *symbol-chars*)
     1 (cons "withmacro" ($ 1)))
    
    (("(\\()(~a~a*)" *symbol-starts* *symbol-chars*)
     2 ($ 1)
     (let ((symbol (find-symbol (string-upcase ($ 2)) :cl)))
       (cond ((special-operator-p symbol)
	      (cons "specialop" ($ 2)))
	     ((macro-function symbol)
	      (cons "stdmacro" ($ 2)))
	     ((function symbol)
	      (cons "stdfun" ($ 2)))
	     (t ($ 2)))))
    
    ;;everything else is a symbol
    (("(~a~a*)" *symbol-starts* *symbol-chars*)
     1 (cons "symbol" ($ 1)))))
