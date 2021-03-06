(in-package :checkmate)

;;------------------------------------------------------------

(defun check (context expression type)
  "Returns typed-expression or errors"
  (let* ((typed-expression (infer context expression))
         (err (unify (type-of-typed-expression typed-expression) type)))
    (when err (error err))
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
    (infer-internal context expression)))

(defun infer-internal (context expression)
  "The type-system equivalent of eval.
   Assumes the expression is macroexpanded"
  (with-slots (infer-atom) (slot-value context 'type-system)
    (if (atom expression)
        (or (funcall infer-atom context expression)
            (error "Could not infer the type for the atom ~s"
                   expression))
        (infer-form context (first expression) (rest expression)))))

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
  (let ((type (designator->type context designator nil)))
    `(truly-the ,type (:construct ,form))))

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
  (let* ((type (designator->type context type-designator nil))
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
  (destructuring-bind (name . args)
      (alexandria:ensure-list function-designator)
    (multiple-value-bind (ftype new-func-name)
        (get-function-type context
                           name
                           (listp function-designator)
                           args)
      (assert ftype ()
              "TType: No function named ~a found in current scope"
              function-designator)
      (let ((function-designator (or new-func-name function-designator)))
        `(truly-the ,ftype (function ,function-designator))))))

(defun infer-function-call (context name arg-forms)
  (let* (;; we say 'partially' as the function may resolve some unknowns
         ;; and we arent taking that into account yet
         (partially-infered-arg-forms
          (mapcar (lambda (arg-form) (infer-internal context arg-form))
                  arg-forms))
         (wip-arg-types
          (mapcar #'type-of-typed-expression
                  partially-infered-arg-forms)))
    (multiple-value-bind (func-type-ref new-func-name)
        (get-function-type context name t wip-arg-types)
      (if func-type-ref
          (let ((name (or new-func-name name))
                (func-type (deref func-type-ref)))
            (destructuring-bind (typed-arg-forms return-type)
                (check-funcall func-type partially-infered-arg-forms
                               `(,name ,@arg-forms))
              `(truly-the
                ,return-type
                (funcall (truly-the ,func-type-ref (function ,name))
                         ,@typed-arg-forms))))
          (error
           "Could not find function for call in scope: ~a~%given context:~a"
           `(,name ,@arg-forms)
           context)))))

(defun infer-funcall (context func-form arg-forms)
  (let* ((arg-len
          (length arg-forms))
         (arg-types
          (let ((tmp (make-array arg-len)))
            (loop
               :for i :below arg-len
               :do (setf (aref tmp i) (make-unknown)))
            tmp))
         (tmp-func-type
          (make-instance
           'tfunction
           :arg-types arg-types
           :return-type (make-unknown)))
         (tmp-func-type-ref
          (take-ref tmp-func-type))
         (typed-func-form
          (check context func-form tmp-func-type-ref))
         (partially-infered-arg-forms
          (mapcar (lambda (arg-form) (infer-internal context arg-form))
                  arg-forms)))
    (destructuring-bind (typed-arg-forms return-type)
        (check-funcall tmp-func-type partially-infered-arg-forms
                       `(funcall ,func-form ,@arg-forms))
      `(truly-the ,return-type
                  (funcall ,typed-func-form
                           ,@typed-arg-forms)))))

(defun check-funcall (func-type partially-inferred-arg-forms full-form)
  (with-slots (arg-types return-type) func-type
    (assert (= (length partially-inferred-arg-forms)
               (length arg-types))
            () "Incorrect number of args in funtion call:~%~s~%expected ~a"
            full-form
            (length arg-types))
    (let* ((typed-arg-forms
            (loop
               :for arg-form :in partially-inferred-arg-forms
               :for arg-type :across arg-types
               :for form-type := (type-of-typed-expression arg-form)
               :do (let ((err (unify form-type arg-type)))
                     (when err (error err)))
               ;; if arg-form-type was unknown it should now have been unified
               :collect `(truly-the ,form-type ,@(cddr arg-form)))))
      (list typed-arg-forms return-type))))

(defun group-declarations (declarations)
  (mapcan #'rest declarations))

(defun infer-lambda-form (context args body)
  (multiple-value-bind (body declarations doc-string)
      (alexandria:parse-body body :documentation t)
    (let* ((args (mapcar #'alexandria:ensure-list args))
           (arg-type-desigs (mapcar #'second args))
           (declarations (group-declarations declarations))
           (processed-arg-types
            (process-arg-specs context arg-type-desigs
                               declarations (make-hash-table)))
           (typed-args (loop
                          :for (name) :in args
                          :for type :across processed-arg-types
                          :collect (list name type)))
           (body-context (add-bindings context typed-args))
           (typed-body (infer body-context `(progn ,@body)))
           (return-type (type-of-typed-expression typed-body)))
      `(truly-the
        ,(take-ref (make-instance
                    'tfunction
                    :arg-types processed-arg-types
                    :return-type return-type))
        (lambda ,typed-args
          ,@(when doc-string (list doc-string))
          ,@declarations
          ,typed-body)))))

(defun process-arg-specs (context
                          arg-type-designators
                          declarations
                          named-unknowns)
  (multiple-value-bind (constraints-lookup constraints)
      (parse-declarations declarations arg-type-designators)
    (let* ((processed-arg-types
            (mapcar
             (lambda (type)
               (internal-designator-to-type context
                                            named-unknowns
                                            constraints-lookup
                                            type))
             arg-type-designators)))
      (loop
         :for constraint :in constraints
         :do (populate-constraint
              context
              constraint
              named-unknowns))
      (values
       (make-array
        (length processed-arg-types)
        :initial-contents processed-arg-types)
       named-unknowns))))

;; {TODO} handle AND types
;; {TODO} this assumes only regular args (no &key &optional etc)
(defun parse-declarations (declarations arg-types)
  (let* ((flat (alexandria:flatten arg-types))
         (arg-unknowns (remove-duplicates
                        (remove-if-not #'unknown-designator-name-p flat)))
         (constraints nil)
         (constraints-lookup (make-hash-table)))
    (loop
       :for decl :in declarations
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

(defun make-function-ttype (context
                            arg-type-designators
                            return-type-designator
                            &key declarations unknowns)
  (multiple-value-bind (arg-types named-unknowns)
      (process-arg-specs
       context arg-type-designators declarations
       (or unknowns (make-hash-table)))
    (let ((return-type
           (or (gethash return-type-designator named-unknowns)
               (find-ttype context return-type-designator))))
      (take-ref (make-instance 'tfunction
                               :arg-types arg-types
                               :return-type return-type)))))

;; (make-function-ttype
;;  (make-check-context 'tables) '(?a ?a) (ttype tables boolean)
;;  '((satisfies disposable ?a)))

;;------------------------------------------------------------
