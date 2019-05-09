(in-package :checkmate)

;;------------------------------------------------------------
;; Type System

(defclass type-system ()
  ((name :initarg :name)
   (infer-atom-name)
   (infer-atom)
   (infer-special-form-name :initform nil)
   (infer-special-form :initform nil)
   (type-expander-name :initform 'default-type-designator-expander)
   (type-expander :initform #'default-type-designator-expander)
   (get-type-spec-name)
   (get-type-spec)
   (get-constraint-spec-name :initform 'default-thing-getter)
   (get-constraint-spec :initform #'default-thing-getter)
   (get-parameter-spec-name :initform 'default-thing-getter)
   (get-parameter-spec :initform #'default-thing-getter)
   (get-top-level-function-type-name :initform 'default-thing-getter)
   (get-top-level-function-type :initform #'default-thing-getter)
   (get-top-level-var-type-name :initform 'default-thing-getter)
   (get-top-level-var-type :initform #'default-thing-getter)))

(defun default-type-designator-expander (type-system designator)
  (declare (ignore type-system))
  designator)

(defun default-thing-getter (type-system x)
  (declare (ignore type-system x))
  nil)

;;------------------------------------------------------------
;; Contexts

(defclass check-context ()
  ((type-system :initarg :type-system)
   (variable-bindings :initform nil :initarg :variable-bindings)
   (parent :initform nil :initarg :parent)
   (root :initform nil :initarg :root)))

(defclass check-context-root (check-context)
  ((user-data :initform nil :initarg :user-data)))

;;------------------------------------------------------------

(defclass checkmate-spec () ())

;;------------------------------------------------------------
;; Types

(defclass user-ttype-spec (checkmate-spec)
  ((name :initarg :name)
   (arg-param-specs :initarg :arg-param-specs)
   (custom-data :initarg :custom-data :initform nil)))

(defclass ttype ()
  ((refs :initform nil)
   (known-complete :initarg :known-complete :initform nil)))

(defclass user-ttype (ttype)
  ((spec :initarg :spec)
   (name :initarg :name)
   (arg-vals :initarg :arg-vals)))

(defclass type-ref ()
  ((target :initarg :target)))

(defclass unknown (ttype)
  ((name :initform (gensym "?UT"))
   (constraints :initform nil :initarg :constraints)))

(defclass tfunction (ttype)
  ((arg-types :initform nil :initarg :arg-types)
   (return-type :initform nil :initarg :return-type)))

;;------------------------------------------------------------
;; Params

(defclass ttype-parameter-spec (checkmate-spec)
  ((name :initarg :name)
   (equal :initarg :equal)
   (equal-name :initarg :equal-name)
   (valid-p :initarg :valid-p)
   (valid-p-name :initarg :valid-p-name)))

(defclass ttype-parameter ()
  ((name :initarg :name)
   (spec :initarg :spec)
   (value :initarg :value)
   (refs :initform nil)))

(defclass param-ref ()
  ((target :initarg :target)))

(defclass unknown-param (ttype-parameter)
  ((name :initform (gensym "?UP"))
   (value :initform nil :initarg :value)))

;;------------------------------------------------------------
;; Constraints

(defclass constraint-spec (checkmate-spec)
  ((name :initarg :name)
   (satisfies-name :initarg :satisfies-name)
   (satisfies :initarg :satisfies)
   (arg-param-specs :initarg :arg-param-specs)
   (custom-data :initarg :custom-data :initform nil)))

(defclass constraint ()
  ((spec :initarg :spec)
   (name :initarg :name)
   (arg-vals :initarg :arg-vals)))

(defclass constraint-ref ()
  ((target :initarg :target)
   (designator :initarg :designator)))

;;------------------------------------------------------------
;; Generalized Function Types

(defclass generalized-function-type ()
  ((type :initarg :type)))
