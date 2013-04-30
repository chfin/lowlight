;;;; lowlight.lisp

(in-package #:lowlight)

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

(defun light-blocks (style input &optional (blocks style))
  (let ((parts (cl-ppcre:split (md-regex blocks) input :with-registers-p t)))
    (apply #'concatenate (cons 'string (wrap (md-wrapper style) 3 parts)))))

(defun light (style input)
  (let ((style-rules (cdr (assoc style *styles*)))
	(step (list input)))
    (loop for rule in style-rules
       do (setf step (process-rule (car rule) (cadr rule)
				   (caddr rule) step)))
    (to-html (remove nil step))))

(defun light-file (style in &key out css title raw)
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
				   (:raw result)))))))))))

(defun light-file-blocks (style in &key out (blocks style))
  (unless out (setf out (make-pathname :type "html" :defaults in)))
  (let ((str (alexandria:read-file-into-string in)))
    (with-open-file (stream out :direction :output :if-exists :supersede)
      (princ (light-blocks style str blocks) stream)))
  t)

(defmacro define-style (style &body clauses)
  (let* ((p-reg (gensym))
	 (rules (mapcar (lambda (clause)
			  (let ((r (car clause)))
			    `(list ,(if (stringp r) r `(format nil ,@r))
				   (1+ ,(cadr clause))
				   (lambda (,p-reg)
				     ,(cons 'list
					    (cons `(car ,p-reg) (cddr clause)))))))
			 clauses)))
    `(macrolet (($ (n) (list 'nth n ',p-reg)))
       (setf *styles* (cons (cons ,style ,(cons 'list rules))
			      (remove ,style *styles* :key #'car))))))
