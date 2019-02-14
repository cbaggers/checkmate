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

(defun infer (context-designator expression)
  "The type-system equivalent of eval.
   Assumes the expression is macroexpanded"
  (let* ((name context-designator)
         (context
          (etypecase name
            (symbol (make-check-context name))
            (type-system (make-check-context name))
            (check-context name))))
    (with-slots (infer-atom) (slot-value context 'type-system)
      (if (atom expression)
          (or (funcall infer-atom context expression)
              (error "Could not infer the type for the atom ~a"
                     expression))
          (infer-form context (first expression) (rest expression))))))

;;------------------------------------------------------------

(defun infer-variable (context expression)
  (let ((type (or (get-binding context expression)
                  (error "Variable ~s is not in scope" expression))))
    `(truly-the ,type ,expression)))

;;------------------------------------------------------------

(defun infer-form (context name args)
  (case name
    (checkmate.lang:construct
     (assert (= (length args) 2))
     (infer-construct context (first args) (second args)))
    (checkmate.lang:quote
     (assert (= (length args) 1))
     (infer-quote-form context (first args)))
    (checkmate.lang:function
     (assert (= (length args) 1))
     (infer-function-form context (first args)))
    (checkmate.lang:progn
      (infer-progn context args))
    (checkmate.lang:funcall
     (infer-funcall context (first args) (rest args)))
    (checkmate.lang:truly-the
     (assert (= (length args) 2))
     (infer-truly-the context (first args) (second args)))
    (checkmate.lang:the
     (assert (= (length args) 2))
     (infer-the context (first args) (second args)))
    (checkmate.lang:lambda
     (infer-lambda-form context (first args) (rest args)))
    (checkmate.lang:let
     (infer-let-form context (first args) (rest args)))
    (otherwise
     (or (with-slots (infer-special-form)
             (slot-value context 'type-system)
           (when infer-special-form
             (funcall infer-special-form context name args)))
         (infer-function-call context name args)))))

;;------------------------------------------------------------

(defun infer-quote-form (context quoted-expression)
  (error "Quoted expressions not implemented yet~%expression:~s~%context:~s"
         `(quote ,quoted-expression)
         context))

(defun infer-construct (context designator form)
  ;; Acts as no-op. The form is correctly types so return as is
  (let ((type (designator->type context designator)))
    `(truly-the ,type ,form)))

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

(defun infer-truly-the (context type form)
  ;; Acts as no-op. The form is correctly types so return as is
  (declare (ignore context))
  (check-type type type-ref)
  `(truly-the ,type ,form))

(defun infer-the (context type-designator form)
  (let* ((type (designator->type context type-designator))
         (typed-form (check context form type)))
    (assert (eq 'truly-the (first typed-form)))
    `(truly-the ,type ,(third typed-form))))

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

(defun infer-function-form (context function-designator)
  (let ((ftype (get-function-type context function-designator)))
    (assert ftype () "TType: No function named ~a found in current scope"
            function-designator)
    `(truly-the ,ftype (function ,function-designator))))

(defun infer-function-call (context name arg-forms)
  (let ((func-type-ref (get-function-type context name)))
    (if func-type-ref
        (let ((func-type (deref func-type-ref)))
          (destructuring-bind (typed-arg-forms return-type)
              (check-funcall context func-type arg-forms
                             `(,name ,@arg-forms))
            `(truly-the ,return-type
                        (funcall (truly-the ,func-type-ref (function ,name))
                                 ,@typed-arg-forms))))
        (error "Could not find function for call in scope: ~a~%given context:~a"
               `(,name ,@arg-forms)
               context))))

(defun infer-funcall (context func-form arg-forms)
  (let* ((arg-len (length arg-forms))
         (arg-types (loop
                       :repeat arg-len
                       :collect (make-unknown)))
         (check-type (make-instance
                      'tfunction
                      :arg-types arg-types
                      :return-type (make-unknown)))
         (check-type-ref (take-ref check-type))
         (typed-func-form (check context func-form check-type-ref)))
    (destructuring-bind (typed-arg-forms return-type)
        (check-funcall context check-type arg-forms
                       `(funcall ,func-form ,@arg-forms))
      `(truly-the ,return-type
                  (funcall ,typed-func-form
                           ,@typed-arg-forms)))))

(defun check-funcall (context func-type arg-forms full-form)
  (with-slots (arg-types return-type) func-type
    (assert (= (length arg-forms)
               (length arg-types))
            () "Incorrect number of args in funtion call:~%~s~%expected ~a"
            full-form
            (length arg-types))
    (let* ((typed-arg-forms
            (loop
               :for arg-form :in arg-forms
               :for arg-type :in arg-types
               :collect (check context arg-form arg-type))))
      (list typed-arg-forms return-type))))

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
                context args constraints-lookup named-unknowns)))
          (loop
             :for constraint :in constraints
             :do (populate-constraint
                  (slot-value context 'type-system)
                  constraint
                  named-unknowns))
          (let* ((body-context (add-bindings context processed-args))
                 (typed-body (infer body-context `(progn ,@body)))
                 (arg-types (mapcar #'second processed-args))
                 (return-type (type-of-typed-expression typed-body))
                 (typed-args (loop
                                :for (name) :in args
                                :for type :in arg-types
                                :collect (list name type))))
            `(truly-the
              ,(take-ref (make-instance 'tfunction
                                        :arg-types arg-types
                                        :return-type return-type))
              (lambda ,typed-args
                ,@(when doc-string (list doc-string))
                ,@declarations
                ,typed-body))))))))

(defun process-function-arg-specs (context
                                   arg-specs
                                   constraints
                                   named-unknowns)
  (loop
     :for spec :in arg-specs
     :for (name type) := spec
     :collect (list name
                    (if type
                        (internal-designator-to-type context
                                                     named-unknowns
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
`                             (cons constraint
                                   (gethash target
                                            constraints-lookup))))))))
    (values constraints-lookup constraints)))

;;------------------------------------------------------------
