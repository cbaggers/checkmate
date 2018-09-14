(in-package :checked-cl)

(cl:defmacro defn (name typed-lambda-list return-type &body body)
  (cl:declare (cl:ignore return-type))
  (cl:let ((expanded (expand-fully body))
           (untyped-lambda-list (extract-untyped-lambda-list
                                 typed-lambda-list)))
    `(cl:defun ,name ,untyped-lambda-list
       ,@expanded)))


(defun foo ((a number) (b number))
  (+ a b))
