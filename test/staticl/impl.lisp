(in-package :staticl-impl)

;;------------------------------------------------------------

(defvar *registered-user-types* (make-hash-table :test #'eq))
(defvar *registered-parameter-types* (make-hash-table :test #'eq))
(defvar *registered-constraints* (make-hash-table :test #'eq))
(defvar *registered-top-level-functions* (make-hash-table :test #'eq))

;;------------------------------------------------------------

(defun register-type (spec)
  (let ((name (spec-name spec)))
    (format t "~%;; Registered type ~a" name)
    (setf (gethash name *registered-user-types*) spec)))

(defun register-constraint (spec)
  (let ((name (spec-name spec)))
    (format t "~%;; Registered constraint ~a" name)
    (setf (gethash name *registered-constraints*) spec)))

(defun register-parameter-type (spec)
  (let ((name (spec-name spec)))
    (format t "~%;; Registered param type ~a" name)
    (setf (gethash name *registered-parameter-types*) spec)))

(defun register-top-level-function (func-name type)
  (format t "~%;; Registered function ~a" func-name)
  (setf (gethash func-name *registered-top-level-functions*)
        (generalize type)))

;;------------------------------------------------------------

(define-type-system staticl)

(defmethod get-type-spec ((type-system staticl) designator)
  (let ((principle-name (first (alexandria:ensure-list designator))))
    (or (gethash principle-name *registered-user-types*)
        (error "Could not identify type for designator: ~a"
               designator))))

(defmethod get-parameter-spec ((type-system staticl) name)
  (or (gethash name *registered-parameter-types*)
      (error
       "define-ttype: ~a is not valid designator arg type.~%valid:~a"
       name (alexandria:hash-table-keys *registered-parameter-types*))))

(defmethod get-constraint-spec ((type-system staticl) designator)
  (let ((principle-name (first (alexandria:ensure-list designator))))
    (or (gethash principle-name
                 *registered-constraints*)
        (error "Could not identify constraint for designator: ~a"
               designator))))

(defmethod get-top-level-function-type ((type-system staticl) name)
  (or (gethash name *registered-top-level-functions*)
      (error "Could not function for name: ~a" name)))

;;------------------------------------------------------------

(defmacro define-ttype (designator
                        &body rest
                        &key where custom-spec-data)
  (declare (ignore rest))
  (destructuring-bind (name . rest) (uiop:ensure-list designator)
    (declare (ignore rest))
    (let ((spec (register-type
                 (make-ttype-spec (find-type-system 'staticl)
                                  designator
                                  where
                                  custom-spec-data))))
      `(progn
         (register-type ,spec)
         ',name))))

(defmacro define-constraint (designator
                             &body rest
                             &key where satifies-this-p
                               custom-spec-data)
  (declare (ignore rest))
  (destructuring-bind (name . rest) (uiop:ensure-list designator)
    (declare (ignore rest))
    (let ((spec (register-constraint
                 (make-constraint-spec (find-type-system 'staticl)
                                       designator
                                       where
                                       satifies-this-p
                                       custom-spec-data))))
      `(progn
         (register-constraint ,spec)
         ',name))))

(defmacro define-parameter-type (name
                                 &body rest
                                 &key valid-p equal)
  (declare (ignore rest))
  (let ((spec (register-parameter-type
               (make-parameter-spec (find-type-system 'staticl)
                                    name
                                    valid-p
                                    equal))))
    `(progn
       (register-parameter-type ,spec)
       ',name)))

(defmacro defn (name args &body body)
  (let ((spec (type-of-typed-expression
                (infer (make-check-context 'staticl)
                       `(lambda ,args ,@body)))))
    (register-top-level-function name spec)
    `(progn
       (register-top-level-function ',name ,spec))))

;;------------------------------------------------------------

(defmethod infer-literal ((type-system staticl) (expression integer))
  `(truly-the ,(checkmate::designator->type type-system 'integer)
              ,expression))

;;------------------------------------------------------------
