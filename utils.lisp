(in-package :checkmate)

(defmacro dbind (lambda-list expression &body body)
  `(destructuring-bind ,lambda-list ,expression ,@body))

(defun last1 (list)
  (car (last list)))
