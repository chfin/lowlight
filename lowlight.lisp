;;;; lowlight.lisp

(in-package #:lowlight.1)

(defun read-into-string (in-stream &key (buffer-size 4096))
  "=> a string containing the whole stream data from `in-stream`
Taken from Alexandria's `read-file-into-string.`"
  (let ((*print-pretty* nil))
    (with-output-to-string (datum)
      (let ((buffer (make-array buffer-size :element-type 'character)))
	(loop
	   :for bytes-read = (read-sequence buffer in-stream)
	   :do (write-sequence buffer datum :start 0 :end bytes-read)
	   :while (= bytes-read buffer-size))))))

(defun make-blocks-regex (blocks)
  (format nil "```(~{~(~a~)~^|~})([\\S\\s]*?)```" blocks))

(defun light-blocks%* (style input output blocks)
  (with-input-from-string (pre "```")
    (with-input-from-string (post (format nil "```~(~a~)" (car blocks)))
      (let* ((stream (make-concatenated-stream pre input post))
	     (lexer (make-instance 'graylex:lexer-input-stream
				   :stream stream
				   :rules (cons (make-blocks-rule blocks)
						(style-rules (get-style style))))))
	(tokenize lexer output)))))

(defun light (style input &optional output)
  "=> a highlighted string or `t`
Highlights `input` which can be either a stream or a string.
The used style is given by `style`.
If `output` (a stream) is given, the result is printed on it,
otherwise the result is returned as a string."
  (cond
    ((stringp input)
     (with-input-from-string (stream input) (light style stream output)))
    ((not output)
     (with-output-to-string (out) (light style input out)))
    (t (process-style (get-style style) input output)
       t)))

(defun light-blocks% (style input output blocks)
  "highlight all blocks in input"
  (princ (ppcre:regex-replace-all (make-blocks-regex blocks) input
				  (lambda (match r1 r2 &rest rest)
				    (declare (ignore match rest))
				    (format nil
					    "<pre><code class=\"~(~a~) ~(~a~)\">~a</code></pre>"
					    style r1 (light style r2)))
				  :simple-calls t)
	 output))

(defun light-blocks (style input &key output (blocks style))
  (cond
    ((streamp input)
     (light-blocks style (read-into-string input) :output output :blocks blocks))
    ((not output)
     (with-output-to-string (out)
       (light-blocks style input :output out :blocks blocks)))
    (t (light-blocks% style input output (if (listp blocks) blocks (list blocks)))
       t)))

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
  (with-open-file (spinneret:*html* out :direction :output :if-exists :supersede)
    (if raw
	(with-open-file (in-strm in)
	  (light style in-strm out-strm))
	(spinneret:with-html
	  (:doctype)
	  (:html (:head (:title (or title (file-namestring in)))
			  (when css (:link :rel "stylesheet" :href css)))
		 (:body
		  (:pre (:code (prog1 nil
				 (with-open-file (in-strm in)
				   (light style in-strm spinneret:*html*))))))))))
  t)

(defun light-file-blocks (style in &key out (blocks style))
  (unless out (setf out (make-pathname :type "html" :defaults in)))
  (with-open-file (out-strm out :direction :output :if-exists :supersede)
    (with-open-file (in-strm in)
      (light-blocks style in-strm :output out-strm :blocks blocks))))

;;; a simple language
(define-simple-style :test-lang
  ("\\\"[\\s\\S]*?\\\"" :string)
  ("/\\*[\\s\\S]*?\\*/" :comment)
  ("\\d*" :number)
  ("\\w*" :word)
  ("\\s*" :default))
