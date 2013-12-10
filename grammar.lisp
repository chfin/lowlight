;;;; grammar.lisp

(in-package #:lowlight.1)

(defvar *token-printer*)
(defvar *parser*)

(defclass cfg-style (simple-style)
  ((parser :reader style-parser
	   :initarg :parser)))

(defun print-token* (class token)
  (if (consp token)
      (progn
	(funcall *token-printer* class (car token))
	(mapcar (lambda (a)
		  (funcall *token-printer* (car a) (cdr a)))
		(cdr token)))
      (funcall *token-printer* class token)))

;;; style definition

(defun maprec (fn tree &optional depth)
  (if (or (not depth) (< 0 depth))
      (mapcar (lambda (elt)
		(if (listp elt)
		    (maprec fn elt (when depth (1- depth)))
		    (funcall fn elt)))
	      tree)
      tree))

(defun tree-sub (table tree &optional depth)
  (maprec (lambda (elt)
	    (let ((sub (assoc elt table)))
	      (if sub (cdr sub) elt)))
	  tree depth))

(defun print-if (cnd obj &optional stream)
  (if cnd
      (print obj stream)
      obj))

(defun inject-ignorables (terminals ignores productions)
  "=> a list of productions
Takes a list of terminals, a list of ignorables, and a list of productions in cl-yacc syntax.
Transforms the productions to allow an arbitrary number of ignorables after each terminal."
  (let ((gensyms (mapcar (lambda (tm) (cons tm (alexandria:make-gensym tm))) terminals))
	(genign (gensym "ign"))
	(genigns (gensym "igns")))
    (append
     ;; take original productions with gensyms,
     (tree-sub gensyms productions 3)
     ;; prods for ignorables after each terminal,
     (mapcar (lambda (tm)
	       (let ((orig (car tm))
		     (gen (cdr tm)))
		 `(,gen ,orig (,orig ,genigns #'cons))))
	     gensyms)
     ;; and prods for lists of ignorables
     (list `(,genigns (,genign) (,genign ,genigns #'cons))
	   `(,genign ,@(mapcar (lambda (i) `(,i (lambda (a) (cons ,i a)))) ignores))))))

(defun rules-to-grammar (rules productions)
  "=> an ignorable sensitive cl-yacc grammar
Takes style rules and productions and creates a cl-yacc grammar.

Rules may contain forms like `(:other-tokens ...)`, each containing a list of keywords
that do not appear as a token keyword in other rules (e.g. `(:other-tokens :a :b :c)`).
Those are added to the list of terminals for the production rules.

Productions may contain one form `(:toplevel ...)` containing a list of toplevel
non-terminals (e.g. `(:toplevel a b c)`).
If no such form is given, all nontermials on the left-hand side of the given productions
will become toplevel.
Productions may also contain one form `(:ignore ...)` containing a list of ignorable tokens.
Those may occur at any positition and are preserved, if all tokens are printed with
`print-token*`.
If no `:ignore` form is given, it defaults to `'(:default)`."
  ;; get all terminals
  (let* ((terminals (remove-if-not (alexandria:of-type 'alexandria:string-designator)
				   (remove-duplicates (alexandria:mappend #'cdr rules))))
	 (toplevels (or (cdr (assoc :toplevel productions))
			(mapcar #'car productions)))
	 (just-prods (remove :toplevel (remove :ignore productions :key #'car) :key #'car))
	 (ignores (or (cdr (assoc :ignore productions))
		      '(:default)))
	 (startsym (gensym "start"))
	 (startsym* (gensym "start*")))
    `((:start-symbol ,startsym)
      (:terminals ,(append ignores terminals))
      ;;start with a list of toplevel productions
      (,startsym nil
		 (,startsym* ,startsym (lambda (a b)
					 (declare (ignore a b))
					 nil)))
      ;;toplevel prods are explicit prods plus a default prod per terminal
      (,startsym* ,@(mapcar (lambda (tm)
			      `(,tm (lambda (a)
				      (lowlight.1::print-token* ,tm a))))
			    (append ignores terminals))
		  ,@toplevels)
      ;;add explicit prods with whitespace function
      ,@(inject-ignorables terminals ignores just-prods))))

(defmacro make-cfg-style% (name rules productions)
  (let ((rules* (mapcar (lambda (rule)
			  (if (listp (car rule))
			      `(cons (format nil ,@(car rule))
				     ,(cadr rule))
			      `(cons ,(car rule) ,(cadr rule))))
			(remove :other-tokens rules :key #'car))))
    `(let ((lowlight.1::*parser* nil))
       ;;define a parser
       (yacc:define-parser lowlight.1::*parser*
	 (:muffle-conflicts t)
	 ,@(print-if *debug* (rules-to-grammar rules productions)))
       ;;add a style object
       (make-instance 'lowlight.1::cfg-style
		      :rules (list ,@rules*
				   (cons "[\\S\\s]" :default))
		      :parser lowlight.1::*parser*
		      :name ',name))))

(defun make-cfg-style (name rules productions)
  "=> a `cfg-style` object
Creates a new `cfg-style` with the name `name`, the token rules `rules` and the
CFG productions `productions`.
For the structure of `rules` and `productions` see `define-cfg-style`."
  (eval (print `(make-cfg-style% ,name ,rules ,productions))))

(defmacro define-cfg-style (name rules productions)
  "Defines a cfg style with token rules `rules` and CFG productions `productions`.
The resulting `cfg-style` object is registered under `name` which should be a keyword.

`rules` is a list of token rules, each of the form `(regex class)`,
where `regex` is either a string describing a regex or a list with such a string in `car`.
In the latter case the string is taken as a control string for `format`
and the rest of the list is given to it as arguments.
The result is then used as the regex.
`class` can be a keyword or a function. If it is a function, it is called with the token
detected by `regex` and should return a keyword.
Rules may contain forms like `(:other-tokens ...)`, each containing a list of keywords
that do not appear as a token keyword in other rules (e.g. `(:other-tokens :a :b :c)`).
Those are added to the list of terminals for the production rules.
You should use this for token keywords that are generated by functions,
as described above.

`productions` is a list of productions in cl-yacc syntax.
The productions may contain *one* form `(:toplevel ...)` containing a list of toplevel
non-terminals (e.g. `(:toplevel a b c)`).
If no such form is given, all nontermials on the left-hand side of the given productions
will become toplevel.
Productions may also contain *one* form `(:ignore ...)` containing a list of ignorable tokens.
Those may occur at any positition (like whitespace and comments)
and are preserved, if all tokens are printed with `print-token*`.
If no `:ignore` form is given, it defaults to `'(:default)`."
  `(setf lowlight.1::*styles*
	 (cons (cons ',name (make-cfg-style% ,name ,rules ,productions))
	       (remove ',name lowlight.1::*styles* :key #'car))))

;;; stream processing

(defmethod process-style ((style cfg-style) input output)
  "Process a stream with a `style`."
  (with-slots (rules parser) style
    (let ((lexer (make-instance 'graylex:lexer-input-stream
				:stream input
				:rules rules
				:buffer-size *lexer-buffer-size*))
	  ;; bind *token-printer* to print html to output
	  (*token-printer* (lambda (class token)
			     (if (eq class :default)
				 (format output "~a" token)
				 (format output "<span class=\"~(~a~)\">~a</span>" class
					 (cl-who:escape-string-minimal token))))))
      ;; to highlight, parse the token stream and let the grammar print the tokens,
      ;; since its productions contain print-token* in the right places.
      (yacc:parse-with-lexer (lambda ()
			       (multiple-value-bind (class token)
				   (graylex:stream-read-token lexer)
				 (if (functionp class)
				     (values (funcall class token) token)
				     (values class token))))
			     parser))))
