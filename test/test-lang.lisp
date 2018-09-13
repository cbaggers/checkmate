(in-package :checkmate)

(define-parameter-type integer
  :valid-p #'integerp
  :equal '=)

(define-ttype boolean
  :custom-spec-data ((implements . (disposable))))

(define-ttype integer)

(define-ttype (unordered-set type size)
  :where ((size integer))
  :custom-spec-data ((implements . (disposable))))

(define-constraint disposable
  :satifies-this-p (lambda (this type-ref)
                     (declare (ignore this))
                     (let ((implements
                            (cdr (assoc 'implements
                                         (custom-data type-ref)))))
                       (find 'disposable implements))))

(define-constraint (breen foo)
  :satifies-this-p (lambda (this type-ref)
                     (declare (ignore this))
                     (print (list :breen> type-ref))
                     t))
