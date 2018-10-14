;;;; package.lisp

(uiop:define-package #:checkmate
    (:use #:cl)
  (:export :define-type-system
           :find-type-system

           :get-type-spec
           :get-parameter-spec
           :get-constraint-spec
           :get-top-level-function-type
           :spec-name
           :spec-custom-data

           :make-check-context
           :make-ttype-spec
           :make-constraint-spec
           :make-parameter-spec

           :ttype
           :ttype-of
           :type-of-typed-expression

           :check
           :infer
           :infer-literal
           :construct
           :generalize

           :type-system
           :check-context
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

           :truly-the))
