(in-package :checkmate)

;; Cute but gross
;;
;; (defmacro define-special-form ((match-var pattern) &body body)
;;   body
;;   (let ((pattern (eval `(let ((_ '__)) ,pattern))))
;;     `(defun foop (form)
;;        (let ((,match-var (match-special-form form ',pattern)))
;;          ,@body))))
;;
;; (defun match-special-form (form pattern)
;;   pattern
;;   form)
;;
;; (define-special-form (match `(let (&rest (_ ,_)) &rest ,_))
;;   (declare (ignore match))
;;   )


(defmacro define-special-form (pattern (match-var) &body body)
  `(defun foop (form)
     (let ((,match-var (match-special-form form ',pattern)))
       ,@body)))

(defun match-special-form (form pattern)
  pattern
  form)

(define-special-form (let (&rest (_ ?)) &rest ?)
    (match)
  (declare (ignore match))
  )

(define-special-form (if ? ? &optional ?)
    (match)
  (destructuring-bind (test then &optional else) (rest match)
    (if else
        (let* ((then-type (cm-type then))
               (else-type (cm-type else)))
          ;; vv this would be checkmate-cl:if
          `(cl:if ,test
                  (the (or ,then-type ,else-type) ,then)
                  (the (or ,then-type ,else-type) ,else)))
        (let* ((then-type (cm-type then)))
          ;; vv this would be checkmate-cl:if
          `(cl:if ,test
                  (the (or ,then-type void) ,then)
                  (the (or ,then-type void) (construct void nil)))))))

(define-special-form (progn &rest ?)
    (match)
  (declare (ignore match))
  )

(defun cm-type (opaque-code-block)
  opaque-code-block)
