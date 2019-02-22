(in-package :checkmate)

;;------------------------------------------------------------

(defun copy-type (type-ref)
  (check-type type-ref type-ref)
  (%copy-type (deref type-ref) (make-hash-table)))

(defun %copy-type (type seen)
  (etypecase type
    (unknown
     (if (gethash type seen)
         (take-ref (gethash type seen))
         (with-slots (constraints) type
           (let ((new (make-unknown
                       (mapcar (lambda (c)
                                 (%copy-constraint c seen))
                               constraints))))
             (setf (gethash type seen) (deref new))
             new))))
    (tfunction
     (take-ref
      (or (gethash type seen)
          (with-slots (arg-types return-type known-complete) type
            (let ((wip (make-instance
                        'tfunction
                        :known-complete known-complete)))
              (setf (gethash type seen) wip)
              (setf (slot-value wip 'arg-types)
                    (map 'vector
                         (lambda (a) (%copy-type (deref a) seen))
                         arg-types))
              (setf (slot-value wip 'return-type)
                    (%copy-type (deref return-type) seen))
              wip)))))
    (user-ttype
     (take-ref
      (or (gethash type seen)
          (with-slots (spec name arg-vals known-complete) type
            (let ((wip (make-instance 'user-ttype
                                      :spec spec
                                      :name name
                                      :known-complete known-complete)))
              (setf (gethash type seen) wip)
              (setf (slot-value wip 'arg-vals)
                    (copy-type-args arg-vals seen))
              wip)))))))

(defun copy-type-args (args seen)
  (labels ((copy-arg (arg)
             (etypecase arg
               (ttype (%copy-type arg seen))
               (ttype-parameter (%copy-param arg seen)))))
    (let ((result (make-array (length args))))
      (loop
         :for arg :across args
         :for i :from 0
         :do (setf (aref result i)
                   (copy-arg (deref arg))))
      result)))

(defun %copy-param (param seen)
  (take-ref
   (etypecase param
     (unknown-param
      (or (gethash param seen)
          (with-slots (value) param
            (setf (gethash param seen)
                  (make-instance 'unknown-param :value value)))))
     (ttype-parameter
      (or (gethash param seen)
          (with-slots (name spec value) param
            (setf (gethash param seen)
                  (make-instance 'ttype-parameter
                                 :name name
                                 :spec spec
                                 :value value))))))))

;; {TODO} constraint-ref holds designator, should probably copy
(defun copy-constraint (constraint-ref)
  (check-type constraint-ref constraint-ref)
  (%copy-constraint constraint-ref
                    (make-hash-table)))

(defun %copy-constraint (constraint-ref seen)
  (check-type constraint-ref constraint-ref)
  (let ((constraint (deref constraint-ref)))
    (make-instance
     'constraint-ref
     :designator (copy-tree (slot-value constraint-ref 'designator))
     :target
     (or (gethash constraint seen)
         (with-slots (spec name arg-vals) constraint
           (let ((wip (make-instance 'constraint
                                     :spec spec
                                     :name name)))
             (setf (gethash constraint seen) wip)
             (setf (slot-value wip 'arg-vals)
                   (copy-type-args arg-vals seen))
             wip))))))

;;------------------------------------------------------------
