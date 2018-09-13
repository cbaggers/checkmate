(in-package :checkmate)

;;------------------------------------------------------------

(defun check (context expression type)
  "Returns typed-expression or errors"
  (let ((typed-expression (infer context expression)))
    (unify (type-of-typed-expression typed-expression)
           type
           t)
    typed-expression))

;;------------------------------------------------------------

(defun infer (context expression)
  "The type-system equivalent of eval.
   Assumes the expression is macroexpanded"
  (cond
    ((or (eq expression t)
         (eq expression nil))
     (infer-literal context expression))
    ((symbolp expression)
     (infer-variable context expression))
    ((listp expression)
     (infer-form context
                 (first expression)
                 (rest expression)))
    (t
     (infer-literal context expression))))

;;------------------------------------------------------------

(defgeneric infer-literal (context expression)
  (:method (c e)
    (error "Could not infer a type for literal ~a given env ~a" c e)))

(defmethod infer-literal (context (expression symbol))
  (declare (ignore context))
  (assert (or (eq expression t)
              (eq expression nil)))
  `(truly-the ,(designator->type 'boolean) ,expression))

(defmethod infer-literal (context (expression integer))
  (declare (ignore context))
  `(truly-the ,(designator->type 'integer) ,expression))

;;------------------------------------------------------------

(defun infer-variable (context expression)
  (let ((type (or (get-binding context expression)
                  (error "Variable ~s is not in scope" expression))))
    `(truly-the ,type ,expression)))

;;------------------------------------------------------------

(defgeneric infer-form (context name args)
  (:method (context name args)
    (case name
      (if
       (assert (= (length args) 3))
       (infer-if context (first args) (second args) (third args)))
      (construct
       (assert (= (length args) 2))
       (infer-construct context (first args) (second args)))
      (truly-the
       (assert (= (length args) 2))
       (infer-truly-the context (first args) (second args)))
      (the
       (assert (= (length args) 2))
       (infer-the context (first args) (second args)))
      (progn (infer-progn context args))
      (let (infer-let-form context (first args) (rest args)))
      (lambda (infer-lambda-form context (first args) (rest args)))
      (funcall (infer-funcall context (first args) (rest args)))
      (function
       (assert (= (length args) 1))
       (infer-function-form context (first args)))
      (quote
       (assert (= (length args) 1))
       (infer-quote-form context (first args)))
      (infer-function-call context name args)
      (otherwise (error "Could not infer a type for: ~a~%given context:~a"
                        `(,name ,@args)
                        context)))))

;;------------------------------------------------------------

(defun infer-quote-form (context quoted-expression)
  (error "Quoted expressions not implemented yet~%expression:~s~%context:~s"
         `(quote ,quoted-expression)
         context))

(defun infer-function-form (context function-designator)
  (let ((ftype (get-function-type context function-designator)))
    (assert ftype () "TType: No function named ~a found in current scope"
            function-designator)
    `(truly-the ,ftype (function ,function-designator))))

(defun infer-if (context test then else)
  (let* (;; {TODO} support any object in test
         (typed-test (check context test (designator->type 'tboolean)))
         ;; {TODO} can we support 'or' types here?
         (typed-then (infer context then))
         (let-type (type-of-typed-expression typed-then))
         (typed-else (check context else let-type)))
    `(truly-the ,let-type
                (if ,typed-test
                    ,typed-then
                    ,typed-else))))

(defun infer-construct (context designator form)
  ;; Acts as no-op. The form is correctly types so return as is
  (declare (ignore context))
  (let ((type (designator->type designator)))
    `(truly-the ,type ,form)))

(defun infer-truly-the (context type form)
  ;; Acts as no-op. The form is correctly types so return as is
  (declare (ignore context))
  (check-type type type-ref)
  `(truly-the ,type ,form))

(defun infer-the (context type-designator form)
  (let* ((type (designator->type type-designator))
         (typed-form (check context form type)))
    (assert (eq 'truly-the (first typed-form)))
    `(truly-the ,type ,(third typed-form))))

(defun infer-progn (context body)
  (let* ((butlast
          (loop
             :for form :in (butlast body)
             :collect (infer context form)))
         (last1 (infer context (car (last body)))))
    `(truly-the ,(type-of-typed-expression last1)
                (progn
                  ,@butlast
                  ,last1))))

(defun infer-let-form (context declarations body)
  (destructuring-bind (inferred-decls type-pairs)
      (loop
         :for (decl-name decl-form) :in declarations
         :for typed-form := (infer context decl-form)
         :for type := (type-of-typed-expression typed-form)
         :collect `(,decl-name ,typed-form) :into typed-decls
         :collect `(,decl-name ,type) :into type-pairs
         :finally (return (list typed-decls type-pairs)))
    (let* ((body-context (add-bindings context type-pairs))
           (typed-body (infer body-context `(progn ,@body))))
      `(truly-the ,(type-of-typed-expression typed-body)
                  (let ,inferred-decls
                    ,typed-body)))))

(defun infer-funcall (context func-form arg-forms)
  (let* ((arg-len (length arg-forms))
         (arg-types (loop
                       :repeat arg-len
                       :collect (make-unknown)))
         (ret-type (make-unknown))
         (check-type (take-ref (make-instance
                                'tfunction
                                :arg-types arg-types
                                :return-type ret-type)))
         (typed-func-form (check context func-form check-type))
         (typed-arg-forms
          (loop
             :for arg-form :in arg-forms
             :for arg-type :in arg-types
             :collect (check context arg-form arg-type))))
    (assert (= (length arg-forms)
               (length (slot-value
                        (deref (type-of-typed-expression typed-func-form))
                        'arg-types)))
            () "Incorrect number of args in funcall~%~s"
            `(funcall ,func-form ,@arg-forms))
    `(truly-the ,ret-type
                (funcall ,typed-func-form
                         ,@typed-arg-forms))))

(defun infer-lambda-form (context args body)
  (multiple-value-bind (body declarations doc-string)
      (alexandria:parse-body body :documentation t)
    (let* ((args (mapcar #'alexandria:ensure-list args))
           (named-unknowns
            (make-hash-table)))
      (multiple-value-bind (constraints-lookup constraints)
          (parse-declarations declarations args)
        (let ((processed-args
               (process-function-arg-specs
                args constraints-lookup named-unknowns)))
          (loop
             :for constraint :in constraints
             :do (populate-constraint constraint named-unknowns))
          (let* ((body-context (add-bindings context processed-args))
                 (typed-body (infer body-context `(progn ,@body)))
                 (arg-types (mapcar #'second processed-args))
                 (return-type (type-of-typed-expression typed-body)))
            `(truly-the
              ,(take-ref (make-instance 'tfunction
                                        :arg-types arg-types
                                        :return-type return-type))
              (lambda ,args
                ,@(when doc-string (list doc-string))
                ,@declarations
                ,typed-body))))))))

(defun process-function-arg-specs (arg-specs constraints named-unknowns)
  (loop
     :for spec :in arg-specs
     :for (name type) := spec
     :collect (list name
                    (if type
                        (internal-designator-to-type named-unknowns
                                                     constraints
                                                     type)
                        (let ((constraints-for-this
                               (gethash type constraints)))
                          (make-unknown constraints-for-this))))))

;; {TODO} handle AND types
;; {TODO} this assumes only regular args (no &key &optional etc)
(defun parse-declarations (declaration-forms args)
  (let* ((flat (alexandria:flatten (mapcar #'second args)))
         (arg-unknowns (remove-duplicates
                        (remove-if-not #'unknown-designator-name-p flat)))
         (merged (mapcar #'second declaration-forms))
         (constraints nil)
         (constraints-lookup (make-hash-table)))
    (loop
       :for decl :in merged
       :do (ecase (first decl)
             (satisfies
              (let* ((spec (second decl))
                     (targets (cddr decl))
                     (constraint
                      (make-instance 'constraint-ref :designator spec)))
                (push constraint constraints)
                (loop
                   :for target :in targets
                   :do (assert (unknown-designator-name-p target)
                               () "Cannot constrain known type ~a"
                               (second target))
                   :do (assert (find target arg-unknowns))
                   :do (setf (gethash target constraints-lookup)
                             (cons constraint
                                   (gethash target
                                            constraints-lookup))))))))
    (values constraints-lookup constraints)))

;;------------------------------------------------------------