(in-package :checkmate)

;;------------------------------------------------------------
;; Type System

(defclass type-system ()
  ((name :initarg :name)
   (boolean-type-designator :initarg :boolean-type-designator)
   (true-symbol :initarg :true-symbol)
   (false-symbol :initarg :false-symbol)))

;;------------------------------------------------------------
;; Contexts

(defclass check-context ()
  ((type-system :initarg :type-system)
   (function-types :initform nil :initarg :function-types)
   (variable-bindings :initform nil :initarg :variable-bindings)
   (parent :initform nil :initarg :parent)))

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
