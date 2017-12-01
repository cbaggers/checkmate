;;;; checkmate.asd

(asdf:defsystem #:checkmate
  :description "Experiment"
  :author "Chris Bagley (techsnuffle) <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  :depends-on (:fn
               :optima
               :fare-quasiquote-extras
               :fiveam)
  :serial t
  :components ((:file "package")
               (:file "readtables")
               (:file "utils")
               (:file "env")))
