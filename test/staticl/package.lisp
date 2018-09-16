;;;; package.lisp

(defpackage #:staticl-impl
  (:use #:cl #:checkmate))

(defpackage #:staticl
  (:use)
  (:import-from :staticl-impl
                ))
