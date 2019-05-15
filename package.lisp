;;;; package.lisp

(uiop:define-package #:checkmate.lang
    (:use)
  (:import-from :cl
                :quote
                :function
                :funcall
                :the
                :lambda
                :let
                :progn)
  (:export :construct
           :quote
           :function
           :funcall
           :the
           :truly-the
           :lambda
           :let
           :progn))

(uiop:define-package #:checkmate
    (:use #:cl)
  (:export :define-type-system
           :find-type-system

           :expand-type-designator
           :get-type-spec
           :get-parameter-spec
           :get-constraint-spec
           :get-top-level-function-type
           :spec-name
           :spec-custom-data
           :ttype-custom-data

           :make-check-context
           :make-ttype-spec
           :make-constraint-spec
           :make-parameter-spec

           :ttype
           :ttype-of
           :ttype-principle-name
           :constraint-principle-name
           :type-of-typed-expression
           :find-ttype
           :find-ttype-by-principle-name
           :make-function-ttype

           :unifies-p
           :check
           :infer
           :infer-variable
           :construct
           :generalize
           :instantiate-function-type
           :complete-p

           :type-system
           :check-context
           :check-context-user-data
           :user-ttype-spec
           :ttype-parameter-spec
           :constraint-spec
           :tfunction
           :user-ttype
           :ttype-parameter
           :constraint
           :type-ref
           :param-ref
           :constraint-ref
           :unknown
           :unknown-param
           :generalized-function-type

           :truly-the

           :match-ttype
           :ematch-ttype
           :dbind-ttype))
