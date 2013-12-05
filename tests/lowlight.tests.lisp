;;;; tests/lowlight.tests.lisp

(in-package #:lowlight.tests)

(def-suite lowlight-all
    :description "Test suite containing all lowlight tests")

(in-suite lowlight-all)

(defparameter *lisp-str*
  "(defun test (bla)
  (if (eq bla :a)
      :b
      (1+ bla)))")

(defparameter *test-str*
  "Dies ist ein Test!
12 \"bla\"
/* ieanrsier
ienrsien */")

(defparameter *blocks-test-str*
  "Blablabla
hier ist ganz viel Zeugs,
aber jetzt wirds spannend:
```test-lang
Dies ist ein Test!
12 \"bla\"
/* ieanrsier
ienrsien */
```
Da ist dann Ende
jetzt nochmal kurz
```tl
bla 123
```")

(test grammar-lisp
  (is (equal
       (light :common-lisp *lisp-str*)
       "(<span class=\"define\">defun</span> <span class=\"defname\">test</span> (<span class=\"symbol\">bla</span>)
  (<span class=\"specialop\">if</span> (<span class=\"stdfun\">eq</span> <span class=\"symbol\">bla</span> <span class=\"keyword\">:a</span>)
      <span class=\"keyword\">:b</span>
      (<span class=\"stdfun\">1+</span> <span class=\"symbol\">bla</span>)))")))

(test simple-lisp
  (is (equal
       (light :cl-simple *lisp-str*)
       "(<span class=\"define\">defun</span> <span class=\"symbol\">test</span> (<span class=\"symbol\">bla</span>)
  (<span class=\"specialop\">if</span> (<span class=\"stdfun\">eq</span> <span class=\"symbol\">bla</span> <span class=\"keyword\">:a</span>)
      <span class=\"keyword\">:b</span>
      (<span class=\"stdfun\">1+</span> <span class=\"symbol\">bla</span>)))")))
