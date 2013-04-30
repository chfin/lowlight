;;;; test.lisp

(in-package #:lowlight)

(defparameter *code* ";; known globals
*read-base*
(let* ((*package* nil)) 'random-symbol)   ; let/let*/lambda are \"special\"

;; known functions, unknown globals
(when (<= 17.69 (get-value))
  (incf *my-global-lol*))

;; number tests
;; all have class \"number\", but depending on type have integer, float, hex, binary
'(80 -69.4 +5 822342.287 #xf40d #b0101)

;; t/nil + strings
(format t \"highlight-lisp.js sucksp: ~a~%\" nil)

;; testing lambda-list specials (&key, &body, etc), multi-line strings,
;; constants, symbols, keywords
(defun test-me (lol &key omg (lol 'wtf))
  (let ((*global* 'ur-mom)
        (strings \"r pretty kewl LOL\")
        (multi-line-strings \"can be kewl
            as well\")
        (+my-constant+ \"wait, constants don't change!!\"))
    (make-instance 'error :message \"OMG ERROR!!!\")))

;; known vs unknown #'functions
(make-hash-table :test #'equal)
(make-hash-table :test #'equalzz)

;; testing known keywords (have class \"known\" as well as \"keyword\")
(loop for x being the :hash-keys of my-hash-table collect x)

(deftype test ())

(defvar *blabla* nil)

#\\A #\\1 #\\Newline #\\\" ")

(defun make-test ()
  (with-open-file (file "/home/christoph/Dev/Lisp/projects/lowlight/test.html"
			:direction :output :if-exists :supersede)
    (let ((spinneret:*html* file))
      (spinneret:with-html
	(:doctype)
	(:html
	 (:head
	  (:title "Some highlighted code")
	  (:link :rel "stylesheet" :href "test.css"))
	 (:body
	  (:h1 "Testcode")
	  (:pre
	   (:code
	    (let ((*print-pretty* nil))
	      (:raw (light :common-lisp *code*)))))))))))

(defun make-test2 ()
  (light-file :common-lisp
	      "/home/christoph/Dev/Lisp/projects/lowlight/lowlight.lisp"
	      :css "test.css"))
