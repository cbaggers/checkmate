(in-package :checkmate)

;; cute idea, but cant represent many constructs (e.g. funcall) and
;; doesnt help with changing between lisp1/2 etc
;;
;; I think it's nice to provide a option on define-type-system to
;; indicate whether lisp1 or lisp2 and Id also like to add a hook
;; which gets given the current package and a symbol and let the
;; user return a new symbol. In this way they should be able to
;; implement their own package system ontop of lisp.
;;
;; We can provide different implementations of progn, lets etc under
;; different names. They can be ugly as it's up to the user of
;; checkmate to expose them how they see fit


(defmacro define-special-form (signature &body body)
  (declare (ignore signature body))
  nil)

(define-special-form
    (the (equal ?0 ?1)
         (if (the boolean test)
             (the ?0 then)
             &optional (the ?1 else))))

(define-special-form
    (the (last ?0)
         (progn &rest (the ?0 form))))

(define-special-form
    ())
