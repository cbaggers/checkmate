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

#+nil
(defn bear ((a ?a))
    (declare (satisfies disposable ?a))
  a)

#+nil
(infer (make-check-context 'staticl)
       '#'bear)

#+nil
(infer (make-check-context 'staticl)
       `(funcall (lambda ((a (unordered-set ?a ?b))
                          (b ?a))
                   (declare (satisfies disposable ?a))
                   (horse b)
                   (horse 1))
                 (construct
                  (unordered-set boolean 10)
                  :foo)
                 t))
