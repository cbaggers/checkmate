;;;; package.lisp
(uiop:define-package #:checkmate
    (:use #:cl #:named-readtables :uiop))

(uiop:define-package #:checkmate.internals
    (:use #:cl #:named-readtables :uiop)
  (:export :expand-fully
           :extract-untyped-lambda-list))

(uiop:define-package #:checked-cl
    (:use :checkmate.internals)
  (:import-from :cl
                :in-package
                :&allow-other-keys
                :&aux
                :&body
                :&environment
                :&key
                :&optional
                :&rest
                :&whole)
  (:export :in-package
           ))
