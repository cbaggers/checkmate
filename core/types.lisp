(in-package :checkmate)

;;------------------------------------------------------------
;; Contexts

(defclass check-context ()
  ((type-system :initarg :type-system)
   (function-types :initform nil :initarg :function-types)
   (variable-bindings :initform nil :initarg :variable-bindings)
   (parent :initform nil :initarg :parent)))

;;------------------------------------------------------------
;; Types

(defclass user-ttype-spec ()
  ((name :initarg :name)
   (arg-param-specs :initarg :arg-param-specs)
   (custom-data :initarg :custom-data :initform nil)))

(defclass ttype ()
  ((refs :initform nil)
   (known-complete :initform nil)))

(defclass user-ttype (ttype)
  ((spec :initarg :spec)
   (name :initarg :name)
   (arg-vals :initarg :arg-vals)
   (known-complete :initarg :known-complete :initform nil)))

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

(defclass ttype-parameter-spec ()
  ((name :initarg :name)
   (unify :initarg :unify)
   (valid-p :initarg :valid-p)))

(defclass ttype-parameter ()
  ((name :initarg :name)
   (spec :initarg :spec)
   (value :initarg :value)
   (refs :initform nil)))

(defclass param-ref ()
  ((target :initarg :target)))

(defclass unknown-param (ttype-parameter)
  ((name :initform (gensym "?UP"))
   (value :initform nil)))

;;------------------------------------------------------------
;; Constraints

(defclass constraint-spec ()
  ((name :initarg :name)
   (init :initarg :init)
   (satisfies :initarg :satisfies)
   (arg-param-specs :initarg :arg-param-specs)
   (custom-data :initarg :custom-data :initform nil)))

(defclass constraint ()
  ((spec :initarg :spec)
   (name :initarg :name)
   (arg-vals :initarg :arg-vals)))

(defclass constraint-ref ()
  ((target :initarg :spec)
   (designator :initarg :designator)))

;;------------------------------------------------------------
;; Generalized Function Types

(defclass generalized-function-type ()
  ((type :initarg :type)))
