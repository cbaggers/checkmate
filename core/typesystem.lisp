(in-package :checkmate)

;;------------------------------------------------------------

(defclass type-system () (name))

(defvar *registered-type-systems*
  (make-hash-table :test #'eq))

(defun register-type-system (name)
  (assert (subtypep 'staticl 'type-system))
  (when (gethash name *registered-type-systems*)
    (warn ";; Redefining type system: ~a" name))
  (setf (gethash name *registered-type-systems*)
        (make-instance name))
  name)

(defmacro define-type-system (name)
  `(progn
     (defclass ,name (type-system)
       ((name :initform ',name)))
     (register-type-system ',name)
     ',name))

(defgeneric get-type-spec (type-system designator))
(defgeneric get-constraint-spec (type-system designator))
(defgeneric get-parameter-spec (type-system name))
(defgeneric get-top-level-function-type (type-system name))
(defgeneric get-top-level-var-type (type-system name))

;;------------------------------------------------------------

(defun find-type-system (name)
  (or (gethash name *registered-type-systems*)
      (error "Checkmate: No known typesystem called ~a" name)))

(defun type-system-name (type-system)
  (check-type type-system type-system)
  (slot-value type-system 'name))

;;------------------------------------------------------------

(defvar *ttype-param-spec*)

(defun %get-parameter-spec (type-system name)
  (if (eq name 'ttype)
      *ttype-param-spec*
      (get-parameter-spec type-system name)))
