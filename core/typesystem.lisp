(in-package :checkmate)

;;------------------------------------------------------------

(defvar *registered-type-systems*
  (make-hash-table :test #'eq))

(defun register-type-system (name)
  (assert (subtypep name 'type-system))
  (when (gethash name *registered-type-systems*)
    (warn ";; Redefining type system: ~a" name))
  (setf (gethash name *registered-type-systems*)
        (make-instance name))
  name)

(defmacro define-type-system
    (name
     &key infer-atom
       infer-special-form
       type-expander
       get-type-spec
       get-constraint-spec
       get-parameter-spec
       get-top-level-function-type
       get-top-level-var-type)
  (assert (and infer-atom (symbolp infer-atom)))
  (assert (or (null infer-special-form) (symbolp infer-special-form)))
  (let ((infer-special-form
         (or infer-special-form 'default-thing-getter))
        ;;
        (get-type-spec
         (or get-type-spec 'default-thing-getter))
        (type-expander
         (or type-expander 'default-type-designator-expander))
        ;;
        (get-constraint-spec
         (or get-constraint-spec 'default-thing-getter))
        (get-parameter-spec
         (or get-parameter-spec 'default-thing-getter))
        (get-top-level-function-type
         (or get-top-level-function-type 'default-thing-getter))
        (get-top-level-var-type
         (or get-top-level-var-type 'default-thing-getter)))
    `(progn
       (defclass ,name (type-system)
         ((name
           :initform ',name)
          (infer-atom-name
           :initform ',infer-atom)
          (infer-atom
           :initform (lambda (x y) (,infer-atom x y)))
          (infer-special-form-name
           :initform ',infer-special-form)
          (infer-special-form
           :initform (lambda (x y z) (,infer-special-form x y z)))
          (get-type-spec-name
           :initform ',get-type-spec)
          (get-type-spec
           :initform (lambda (x y) (,get-type-spec x y)))
          (type-expander-name
           :initform ',type-expander)
          (type-expander
           :initform (lambda (x y) (,type-expander x y)))
          (get-constraint-spec-name
           :initform ',get-constraint-spec)
          (get-constraint-spec
           :initform (lambda (x y) (,get-constraint-spec x y)))
          (get-parameter-spec-name
           :initform ',get-parameter-spec)
          (get-parameter-spec
           :initform (lambda (x y) (,get-parameter-spec x y)))
          (get-top-level-function-type-name
           :initform ',get-top-level-function-type)
          (get-top-level-function-type
           :initform (lambda (w x y z)
                       (,get-top-level-function-type w x y z)))
          (get-top-level-var-type-name
           :initform ',get-top-level-var-type)
          (get-top-level-var-type
           :initform (lambda (x y) (,get-top-level-var-type x y)))))
       (register-type-system ',name)
       ',name)))

;;------------------------------------------------------------

(defun find-type-system (name)
  (or (gethash name *registered-type-systems*)
      (error "Checkmate: No known typesystem called ~a" name)))

(defun type-system-name (type-system)
  (check-type type-system type-system)
  (slot-value type-system 'name))

;;------------------------------------------------------------

(defvar *ttype-param-spec*)

(defun %get-parameter-spec (context name)
  (if (eq name 'ttype)
      *ttype-param-spec*
      (with-slots (get-parameter-spec) (slot-value context 'type-system)
        (or (funcall get-parameter-spec context name)
            (error "Could not find parameter spec for ~a"
                   name)))))
