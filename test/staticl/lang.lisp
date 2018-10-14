(in-package :staticl-impl)

(define-parameter-type integer
  :valid-p integerp
  :equal =)

(define-ttype boolean
  :custom-spec-data ((implements . (disposable))))

(define-ttype integer)

(define-ttype (unordered-set type size)
  :where ((size integer))
  :custom-spec-data ((implements . (disposable))))

(defun is-disposable (this type-ref)
  (declare (ignore this))
  (let ((implements
         (cdr (assoc 'implements
                     (spec-custom-data type-ref)))))
    (find 'disposable implements)))

(define-constraint disposable
  :satifies-this-p is-disposable)

(defun is-breenable (this type-ref)
  (declare (ignore this))
  (print (list :breen> type-ref))
  t)

(define-constraint (breen foo)
  :satifies-this-p is-breenable)

(defn horse ((a ?a))
  a)

(defmethod infer-literal ((type-system staticl) (expression integer))
  `(truly-the ,(designator->type type-system 'integer) ,expression))
