;;;; style-lisp.lisp

(in-package #:lowlight.1)

(defun classify-symbol (str)
  (let ((symbol (find-symbol (string-upcase str) :cl)))
    (if symbol
	(cond ((special-operator-p symbol) :specialop)
	      ((macro-function symbol) :stdmacro)
	      (t :stdfun))
	:symbol)))

(eval-when (:load-toplevel :compile-toplevel)
  (defun print-typedef (def name)
    (print-token* :define def)
    (print-token* :typedefname name))

  (defun print-vardef (def name)
    (print-token* :define def)
    (print-token* :vardefname name))

  (defun print-define (def name)
    (print-token* :define def)
    (print-token* :defname name))

  (defun print-setf-define (def setf name)
    (print-token* :define def)
    (print-token* :setf setf)
    (print-token* :defname name)))

(let ((symbol-reg
       "([^\\s'`,\"\\(\\);:#]|\\|[\\S\\s]*?\\|)([^\\s'`,\"\\(\\);:]|\\|[\\S\\s]*?\\|)*")
      (symbol-chars "([^\\s'`,\"\\(\\);:]|\\|[\\S\\s]*?\\|)")
      (+- "[\\+\\-]?"))
  
  (define-cfg-style :common-lisp
    ((";.*" :comment)
     ("#\\|[\\S\\s]*?\\|#" :comment)
     ("(#'|[,'`]+)" :quote)
     
     ;; literals
     ("\"[\\S\\s]*?[^\\\\]\"" :string)
     ("\"\"" :string)
     ("(?i)#\\\\(space|newline|tab|page|return|linefeed|\\S)" :character)
     (("~a(\\d+|\\d+.\\d+|\\.\\d+)([eE]\\d+)?(?=[\\s'`,\"\\(\\);:])" +-) :number)
     (("(#[box]|#\\d+r)~a\\w+(/\\w+)?(?=[\\s'`,\"\\(\\);:])" +-) :number)
     
     ;;defs
     ("def(class|struct|type)" :deftype)
     ("def(var|parameter|constant)" :defvar)
     (("def~a*" symbol-chars) :define)
     
     ;;symbols
     ("(&key|&optional|&body|&rest|&whole|&allow-other-keys|&aux|&environment|&any)" :lambda)
     (("\\*~a*\\*" symbol-chars) :global)
     (("\\+~a*\\+" symbol-chars) :constant)
     (("with-~a*" symbol-chars) :withmacro)
     ("setf" :setf)
     ((":~a" symbol-reg) :keyword)
     (("(~a::?|#:)?~a" symbol-reg symbol-reg) #'classify-symbol)
     
     ("\\s*" :default)
     ("\\(" :default)
     ("\\)" :default)
     (:other-tokens :specialop :stdmacro :stdfun :symbol))
    
    ;;grammar productions
    ((:toplevel typedef vardef def)
     (:ignore :default :comment)
     (typedef (:deftype name #'print-typedef))
     (vardef (:defvar name #'print-vardef))
     (def
	 (:define name #'print-define)
	 (:define :setf name #'print-setf-define))
     (name :symbol :specialop :stdmacro :stdfun :deftype :defvar
	   :define :global :constant :withmacro :keyword)))
  
  (define-simple-style :cl-simple
    (";.*" :comment)
    ("#\\|[\\S\\s]*?\\|#" :comment)
    ("(#'|[,'`]+)" :quote)
    
    ;; literals
    ("\"[\\S\\s]*?[^\\\\]\"" :string)
    ("\"\"" :string)
    ("(?i)#\\\\(space|newline|tab|page|return|linefeed|\\w)" :character)
    (("~a(\\d+|\\d+.\\d+|\\.\\d+)([eE]\\d+)?(?=[\\s'`,\"\\(\\);:])" +-) :number)
    (("(#[box]|#\\d+r)~a\\w+(/\\w+)?(?=[\\s'`,\"\\(\\);:])" +-) :number)
    
    ;;defs
    ("def(class|struct|type)" :deftype)
    ("def(var|parameter|constant)" :defvar)
    (("def~a*" symbol-chars) :define)
    
    ;;symbols
    ("(&key|&optional|&body|&rest|&whole|&allow-other-keys|&aux|&environment|&any)" :lambda)
    (("\\*~a*\\*" symbol-chars) :global)
    (("\\+~a*\\+" symbol-chars) :constant)
    (("with-~a*" symbol-chars) :withmacro)
    ("setf" :setf)
    ((":~a" symbol-reg) :keyword)
    (("(~a::?|#:)?~a" symbol-reg symbol-reg) #'classify-symbol)))
