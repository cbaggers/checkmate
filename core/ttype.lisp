(in-package :checkmate)

;; {TODO} disallow names to start with ~ or ?

;;------------------------------------------------------------

(defun find-ttype-by-principle-name (type-system-designator
                                     principle-type-name)
  (%find-ttype-by-principle-name
   (type-system-designator->context type-system-designator)
   principle-type-name))

(defun %find-ttype-by-principle-name (context principle-name)
  (check-type principle-name symbol)
  (case principle-name
    ;;
    ;; always use make-unknown
    (unknown
     (error "BUG: Attempt to make unknown type via designator"))
    ;;
    ;; non user type
    (function
     (error "BUG: function types have no principle name"))
    ;;
    ;; is a user type or unknown
    (otherwise
     (with-slots (get-type-spec) (slot-value context 'type-system)
       (let* ((type-spec
               (or (funcall get-type-spec
                            context
                            principle-name)
                   (error "Could not find type name ~a"
                          principle-name))))
         ;; note: to-type returns a ref
         (with-slots (arg-param-specs) type-spec
           (let* ((len (length arg-param-specs))
                  (constructed
                   (loop
                      :for pspec :across arg-param-specs
                      :collect
                        (if (eq (slot-value pspec 'name) 'ttype)
                            (make-unknown nil)
                            (make-unknown-param))))
                  (vals
                   (make-array len :initial-contents constructed)))
             (take-ref
              (make-instance 'user-ttype
                             :spec type-spec
                             :name (slot-value type-spec 'name)
                             :arg-vals vals
                             :known-complete (= len 0))))))))))

(defun find-ttype (type-system-designator type-designator
                   &key unknowns declarations)
  (let ((ctx (type-system-designator->context type-system-designator)))
    (values
     (if unknowns
         (aref (process-arg-specs
                ctx (list type-designator) declarations unknowns)
               0)
         (designator->type ctx type-designator unknowns))
     unknowns)))

(defmacro ttype (type-system-designator designator)
  (designator->type
   (make-check-context (find-type-system type-system-designator))
   designator
   nil))

(defun ttype-of (type-ref)
  (with-slots (target) type-ref
    (designator-from-type target)))

(defun ttype-principle-name (type-ref)
  (with-slots (target) type-ref
    (slot-value target 'name)))

(defun constraint-principle-name (constraint-ref)
  (with-slots (target) constraint-ref
    (slot-value target 'name)))

(defun to-type (type-system spec named-unknowns constraints args)
  (with-slots (arg-param-specs) spec
    (let* ((constructed
            (construct-designator-args type-system
                                       spec
                                       named-unknowns
                                       constraints
                                       args))
           (vals
            (make-array
             (length constructed)
             :initial-contents constructed))
           (already-complete
            (or (= (length vals) 0)
                (every (lambda (x)
                         (complete-p (deref x)))
                       vals))))
      (take-ref
       (make-instance 'user-ttype
                      :spec spec
                      :name (slot-value spec 'name)
                      :arg-vals vals
                      :known-complete already-complete)))))

(defun make-ttype-spec (context
                        designator
                        where
                        custom-spec-data)
  (destructuring-bind (name . designator-args)
      (uiop:ensure-list designator)
    (let* ((req-args
            (parse-ttype-lambda-list designator-args))
           (params
            (loop
               :for arg :in req-args
               :collect (%get-parameter-spec
                         context
                         (or (second (find arg where :key #'first))
                             'ttype)))))
      (make-instance
       'user-ttype-spec
       :name name
       :custom-data custom-spec-data
       :arg-param-specs (make-array (length params)
                                    :initial-contents params)))))

(defun designator-from-type (type)
  (check-type type ttype)
  (%designator-from-type (make-hash-table) type))

(defun %designator-from-type (visited type)
  (flet ((desig (p)
           (if (typep p 'type-ref)
               (%designator-from-type visited (deref p))
               (let ((naked-param (deref p))
                     (value (slot-value (deref p) 'value)))
                 (if (typep naked-param 'unknown-param)
                     (slot-value naked-param 'name)
                     value)))))
    (or (gethash type visited)
        (etypecase type
          (unknown
           (slot-value type 'name))
          (tfunction
           (let ((wip (list 'function nil nil)))
             (setf (gethash type visited) wip)
             (with-slots (arg-types return-type) type
               (setf (second wip)
                     (map 'list
                          (lambda (a)
                            (with-slots (target) a
                              (%designator-from-type visited target)))
                          arg-types))
               (setf (third wip)
                     (with-slots (target) return-type
                       (%designator-from-type visited target)))
               wip)))
          (user-ttype
           (with-slots (name arg-vals) type
             (if (> (length arg-vals) 0)
                 (let* ((arg-spaces (make-list (length arg-vals)))
                        (wip (cons name arg-spaces)))
                   (setf (gethash type visited) wip)
                   (loop
                      :for arg :across arg-vals
                      :for i :from 1
                      :do (setf (nth i wip) (desig arg)))
                   wip)
                 name)))))))

;;------------------------------------------------------------

(defun designator->type (type-system type-designator named-unknowns)
  (let ((constraints (make-hash-table)))
    (internal-designator-to-type type-system named-unknowns constraints
                                 type-designator)))

(defun internal-designator-to-type (context
                                    named-unknowns
                                    constraints
                                    designator)
  (let ((principle-name (if (atom designator)
                            designator
                            (first designator))))
    (case principle-name
      ;;
      ;; always use make-unknown
      (unknown
       (error "BUG: Attempt to make unknown type via designator"))
      ;;
      ;; non user type
      (function
       (assert (= (length designator) 3))
       (take-ref
        (make-instance
         'tfunction
         :arg-types (map 'vector
                         (lambda (x)
                           (internal-designator-to-type
                            context named-unknowns constraints x))
                         (second designator))
         :return-type (internal-designator-to-type
                       context named-unknowns constraints
                       (third designator)))))
      ;;
      ;; is a user type or unknown
      (otherwise
       ;;
       ;; dont allow possibility of creating unknown if named-unknowns is nil
       ;; as this means you got here from a public facing method
       (if (and named-unknowns
                (unknown-designator-name-p designator))
           ;;
           ;; unknown
           (or (gethash designator named-unknowns)
               (setf (gethash designator named-unknowns)
                     (make-unknown (gethash designator constraints))))
           ;;
           ;; user type
           (with-slots (get-type-spec type-expander)
               (slot-value context 'type-system)
             (let* ((expanded-designator
                     (funcall type-expander
                              context
                              designator))
                    (type-spec
                     (or (funcall get-type-spec
                                  context
                                  expanded-designator)
                         (error "Could not find type name ~a"
                                designator))))
               ;;
               (let ((args (unless (atom expanded-designator)
                             (rest expanded-designator))))
                 ;; note: to-type does return a ref
                 (to-type context
                          type-spec
                          named-unknowns
                          constraints
                          args)))))))))

;;------------------------------------------------------------

;; {TODO} I'd like the user to have to specify the type of the designator
;;        argument. I think to start we will restrict it to types, symbols
;;        and numbers.
;; {TODO} where could be a a regular function rather than be generated by
;;        this macro
;;
;; hmm, we really need a way to define a type-system.
;; {TODO} ttype needs to have a load-form, which implies all
;;        elements in the designator need to have that too
;;------------------------------------------------------------
