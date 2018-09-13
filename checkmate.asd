;;;; checkmate.asd

(asdf:defsystem #:checkmate
  :description "A system to help building typecheckers"
  :author "Chris Bagley (Baggers) <chris.bagley@gmail.com>"
  :license "BSD 2 Clause"
  :serial t
  :depends-on (:alexandria)
  :components ((:file "package")))
