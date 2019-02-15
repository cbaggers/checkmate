(in-package :checkmate)

;; {TODO} disallow names to start with ~

;;------------------------------------------------------------

(defun find-ttype (type-system-designator type-designator)
  (designator->type (etypecase type-system-designator
                      (check-context
                       type-system-designator)
                      (type-system
                       (make-check-context type-system-designator))
                      (symbol
                       (make-check-context
                        (find-type-system type-system-designator))))
                    type-designator))

(defmacro ttype (type-system-designator designator)
  (designator->type (make-check-context (find-type-system type-system-designator))
                    designator))

(defun ttype-of (type-ref)
  (with-slots (target) type-ref
    (with-slots (spec) target
      (designator-from-type target))))

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

(defun make-ttype-spec (type-system
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
                         type-system
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
  (flet ((desig (p)
           (if (typep p 'type-ref)
               (designator-from-type (deref p))
               (let ((naked-param (deref p))
                     (value (slot-value (deref p) 'value)))
                 (if (typep naked-param 'unknown-param)
                     (slot-value naked-param 'name)
                     value)))))
    (etypecase type
      (unknown
       (slot-value type 'name))
      (tfunction
       (with-slots (arg-types return-type) type
         `(function ,(map 'list #'ttype-of arg-types)
                    ,(ttype-of return-type))))
      (user-ttype
       (with-slots (name arg-vals) type
         (if (> (length arg-vals) 0)
             (cons name (map 'list #'desig arg-vals))
             name))))))

;;------------------------------------------------------------

(defun designator->type (type-system type-designator)
  (internal-designator-to-type type-system nil nil type-designator))

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
       (take-ref (make-instance
                  'tfunction
                  :arg-types (map 'vector
                                  (lambda (x)
                                    (designator->type context x))
                                  (second designator))
                  :return-type (designator->type
                                context
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
                                  (slot-value context 'type-system)
                                  expanded-designator)
                         (error "Could not find type name ~a"
                                designator))))
               ;;
               (let ((args (when (listp expanded-designator)
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
