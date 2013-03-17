(in-package :doplus)

(defmacro over (enum)
  (let ((en (gensym "ENUMERATION")))
    (list
     `(with (,en ,enum))
     (make-iteration :precondition `(enum:has-next-p ,en) :init `(enum:next ,en) :step `(enum:next ,en)))))
