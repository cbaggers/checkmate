(in-package :checkmate)

;;------------------------------------------------------------

(defmacro dbind-ttype (pattern type &body body)
  (assert pattern)
  (labels ((foo (p symb)
             (cond
               ((and (listp p) (eq (first p) 'lambda))
                `())
               (()
                x))
             ())))
  (let ((gtype (gensym "type")))
    `(let ((,gtype ,type))
       ,@body)))

;; (dbind-ttype (lambda (x y) z) foo
;;   ..)
;;
;; (let ((g0 foo))
;;   (with-slots (arg-types return-type) g0
;;     (let ((x (aref arg-types 0))
;;           (y (aref arg-types 1))
;;           (z return-type))
;;       ..)))

;; (dbind-ttype (lambda (i8 (foo x) y) z) foo
;;   ..)
;;
;; (let ((g0 foo))
;;   (with-slots (arg-types return-type) g0
;;     (assert (eq (aref arg-types 0) 'i8))
;;     (let ((g1 (aref arg-types 1))
;;           (y (aref arg-types 2))
;;           (z return-type))
;;       (with-slots (arg-vals) g1
;;         (let ((x (aref arg-vals 0)))
;;           ..)))))

;; (dbind-ttype (lambda (i8 (foo x) y) boolean) foo
;;   ..)
;;
;; (let ((g0 foo))
;;   (with-slots (arg-types return-type) g0
;;     (assert (eq (aref arg-types 0) 'i8))
;;     (assert (eq return-type 'i8))
;;     (let ((g1 (aref arg-types 1))
;;           (y (aref arg-types 2))
;;           (z return-type))
;;       (with-slots (arg-vals) g1
;;         (let ((x (aref arg-vals 0)))
;;           ..)))))


;; (dbind-ttype (lambda (i8 (foo x) (bar y)) boolean) foo
;;   ..)
;;
;; (let* ((g0 foo))
;;   (with-slots (arg-types return-type) g0
;;     (assert (eq (aref arg-types 0) 'i8))
;;     (assert (eq return-type 'i8))
;;     (let* ((g1 (aref arg-types 1))
;;            (x (with-slots (arg-vals) g1
;;                 (aref arg-vals 0)))
;;            (g2 (aref arg-types 2))
;;            (y (with-slots (arg-vals) g2
;;                 (aref arg-vals 0)))
;;            (z return-type))
;;       ..)))

;;------------------------------------------------------------
