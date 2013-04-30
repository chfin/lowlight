;;;; lowlight.doc.lisp

(in-package #:lowlight.doc)

(defun create-doc ()
  "Creates the documentation (doc/lowlight.html) using cl-gendoc."
  (let* ((*default-pathname-defaults*
	  (asdf:component-pathname (asdf:find-component :lowlight.doc nil)))
	 (in (merge-pathnames "../README.md"))
	 (out (merge-pathnames "intro.md")))
    (print in)
    (print out)
    (lowlight:light-file-blocks :common-lisp in :out out))
  (gendoc:gendoc (:output-filename "lowlight.html" :css "ghs-doc.css" :title "lowlight - a simple syntax highlighter" :output-system :lowlight.doc)
    (:mdf "intro.md")
    (:apiref #:lowlight #:lowlight.doc)))
