;;;; lowlight.lisp

(in-package #:lowlight.0)

(defparameter *styles* nil)

(defun wrap (fn n lst &optional coll)
  (if lst
      (if (= (length coll) (- n 1))
	  (append (funcall fn (reverse (cons (car lst) coll)))
		  (wrap fn n (cdr lst)))
	  (wrap fn n (cdr lst) (cons (car lst) coll)))
      (reverse coll)))

(defun process-substring (regex n wrapper string)
  (wrap wrapper n (cl-ppcre:split regex string
				  :with-registers-p t
				  :omit-unmatched-p t)))

(defun cat-strings (lst &optional strs)
  (if (stringp (car lst))
      (cat-strings (cdr lst) (cons (car lst) strs))
      (let ((next (when lst (cons (car lst) (cat-strings (cdr lst))))))
	(if strs
	    (cons (apply #'concatenate (cons 'string (reverse strs))) next)
	    next))))

(defun process-rule (regex n wrapper lst)
  (cat-strings (apply #'append
		      (mapcar (lambda (i)
				(if (stringp i)
				    (process-substring
				     (cl-ppcre:create-scanner
				      regex :single-line-mode t
				      :case-insensitive-mode t)
				     n wrapper i)
				    (list i)))
			      lst))))

(defun htmlifier (stream)
  (lambda (item)
    (if (consp item)
	(format stream "<span class=\"~a\">~a</span>" (car item)
		(cl-who:escape-string-minimal (cdr item)))
	(format stream "~a" item))))

(defun to-html (lst)
  (with-output-to-string (stream)
    (mapcar (htmlifier stream) lst)))

(defun md-regex (blocks)
  (cl-ppcre:create-scanner
   (format nil "```(~{~a~^|~}).(.*?)```" (if (listp blocks) blocks (list blocks)))
   :single-line-mode t :case-insensitive-mode t))

(defun md-wrapper (style)
  (lambda (parts)
    (list (car parts)
	  (let ((*print-pretty* nil))
	    (spinneret:with-html-string
	      (:pre (:code (:raw (light style (caddr parts))))))))))

(defun light (style input)
  "=> a highlighted string
Highlights `input`.
The used style is given by `style`."
  (let ((style-rules (cdr (assoc style *styles*)))
	(step (list input)))
    (loop for rule in style-rules
       do (setf step (process-rule (car rule) (cadr rule)
				   (caddr rule) step)))
    (to-html (remove nil step))))

(defun light-file (style in &key out css title raw)
  "=> `t`
Highlights the file given by `in`.
The used style is denoted by `style`.
If given, the result is written to the file `out`,
otherwise `in` is used, with the file ending replaced by html.
If given, `css` is used as the href parameter to a css relation
and `title` is used as the page title.
If `raw` is `t` the highlighted code is *not* wrapped into a html skeleton."
  (unless out (setf out (make-pathname :type "html" :defaults in)))
  (let* ((str (alexandria:read-file-into-string in))
	 (result (light style str)))
    (with-open-file
	(spinneret:*html* out :direction :output :if-exists :supersede)
      (if raw
	  (princ result spinneret:*html*)
	  (spinneret:with-html
	    (:doctype)
	    (:html (:head (:title (or title (file-namestring in)))
			  (when css (:link :rel "stylesheet" :href css)))
		   (:body
		    (:pre (:code (let ((*print-pretty* nil))
				   (:raw result))))))))))
  t)

(defun light-blocks (style input &optional (blocks style))
  "=> a highlighted string
Takes all blocks between three backticks followed by a language specifier, and three closing backticks.
The language specifiers are given by `blocks` which is either a single item or a list.
The block markers are converted to strings by `~a` and matched case-independently
(so you can use keywords here, `:common-lisp` matches `` ```common-lisp ``)."
  (let ((parts (cl-ppcre:split (md-regex blocks) input :with-registers-p t)))
    (apply #'concatenate (cons 'string (wrap (md-wrapper style) 3 parts)))))

(defun light-file-blocks (style in &key out (blocks style))
  "=> `t`
Highlights all blocks (see [`light-blocks`](#apiref-light-blocks)).
`in` and `out` behave similar to [`light-file`](#apiref-light-file)."
  (unless out (setf out (make-pathname :type "html" :defaults in)))
  (let ((str (alexandria:read-file-into-string in)))
    (with-open-file (stream out :direction :output :if-exists :supersede)
      (princ (light-blocks style str blocks) stream)))
  t)

(defmacro define-style (style &body clauses)
  "Defines a new style.
`style` is the name of the style, typically a keyword.
`clauses` are lists of the form `(regex n result*)`,
where `regex` is either a string literal or a list
beginning with a format string followed by format parameters.
`n` is the number of registers in the regex (which must be constant!).
The results return the highlighted string parts, which are either strings
 (not highlighted) or a cons cell,
where the `car` is the css class and the `cdr` is the highlighted string.
Inside the result clauses use the local macro `$` to access the regex registers,
so `($ 1)` returns the first register.
Note, that all text matched by the regex must appear in the results,
otherwise it will be discarded.
So if you want to highlight all `,` preceded by a `:`,
the clause should look like this:
`(\"(:)(,)\" 2 ($ 1) (cons \"comma\" ($ 2)))`."
  (let* ((p-reg (gensym))
	 (rules (mapcar (lambda (clause)
			  (let ((r (car clause)))
			    `(list ,(if (stringp r) r `(format nil ,@r))
				   (1+ ,(cadr clause))
				   (lambda (,p-reg)
				     ,(cons 'list
					    (cons `(car ,p-reg) 
						  (cddr clause)))))))
			clauses)))
    `(macrolet (($ (n) (list 'nth n ',p-reg)))
       (setf lowlight.0::*styles* (cons (cons ,style ,(cons 'list rules))
				      (remove ,style *styles* :key #'car))))))
