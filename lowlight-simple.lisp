;;;; simple.lisp

(in-package #:lowlight.1)

(defclass simple-style ()
  ((rules :reader style-rules
	  :initarg :rules)
   (name :reader style-name
	 :initarg :name)))

(defmethod get-style ((style simple-style))
  style)

(defmethod style-classes ((style simple-style))
  (remove-if #'functionp
	     (remove-duplicates
	      (remove :default
		      (mapcar #'cdr (style-rules style))))))

;;; style definition

(defun make-simple-style (name rules)
  "=> a `simple-style` object
Creates a new `simple-style` with the name `name`.
`rules` is a list of token rules, each of the form described at `define-simple-style`."
  (make-instance 'simple-style :rules rules :name name))

(defmacro define-simple-style (name &body rules)
  "Defines a simple style with the token rules `rules`.
The resulting `simple-style` object is registered under `name` which should be a keyword.

`rules` is a list of token rules, each of the form `(regex class)`,
where `regex` is either a string describing a regex or a list with such a string in `car`.
In the latter case the string is taken as a control string for `format`
and the rest of the list is given to it as arguments.
The result is then used as the regex.
`class` can be a keyword or a function. If it is a function, it is called with the token
detected by `regex` and should return a keyword."
  (let ((rules (mapcar (lambda (rule)
			 (if (listp (car rule))
			     `(cons (format nil ,@(car rule))
				    ,(cadr rule))
			     `(cons ,(car rule) ,(cadr rule))))
		       rules)))
    `(setf lowlight.1::*styles*
	   (cons (cons ',name (make-simple-style ',name
						(list ,@rules (cons "[\\S\\s]" :default))))
		 (remove ',name lowlight.1::*styles* :key #'car)))))

;;; stream processing

(defun print-token (class token output)
  (when (functionp class)
    (setf class (funcall class token)))
  (if (eq class :default)
      (format output "~a" (cl-who:escape-string-minimal token))
      (format output "<span class=\"~(~a~)\">~a</span>" class
	      (cl-who:escape-string-minimal token))))

(defun tokenize (lexer output)
  (let ((res nil))
    (do ((token (multiple-value-list (graylex:stream-read-token lexer))
		(multiple-value-list (graylex:stream-read-token lexer))))
	((null (car token)) (reverse res))
      (print-token (car token) (cadr token) output))))

(defmethod process-style ((style simple-style) input output)
  "Process a stream with a `simple-style`."
  (let ((lexer (make-instance 'graylex:lexer-input-stream
				   :stream input
				   :rules (style-rules style)
				   :buffer-size *lexer-buffer-size*)))
    (tokenize lexer output)))
