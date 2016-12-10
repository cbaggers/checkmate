;;;; checkmate.asd

(asdf:defsystem #:checkmate
  :description "Describe checkmate here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "env")
               (:file "macros")
               (:file "base")))
