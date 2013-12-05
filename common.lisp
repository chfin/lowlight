;;;; common.lisp

(in-package #:lowlight.1)


(defvar *styles* nil
  "An alist mapping from style keywords to style definitions.")

(defvar *lexer-buffer-size* 1024
  "controlling knob for the graylex buffer size")

;;; get a style by keyword or itself

(defgeneric get-style (style)
  (:documentation "=> a style denoted by `style` being either a key or an actual style"))

(defmethod get-style (style)
  (cdr (assoc style *styles*)))

;;; get a styles name

(defgeneric style-name (style)
  (:documentation "=> the name of `style`"))

(defmethod style-name (style)
  style)

;;; get information about a styles classes

(defgeneric style-classes (style)
  (:documentation "=> the set of token classes generated by `style`"))

(defmethod style-classes (style)
  (let ((s (get-style style)))
    (unless (equal style s)
      (style-classes s))))

;;; process different styles

(defgeneric process-style (style input output))
