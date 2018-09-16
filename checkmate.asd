;;;; checkmate.asd

(asdf:defsystem #:checkmate
  :description "A system to help building typecheckers"
  :author "Chris Bagley (Baggers) <chris.bagley@gmail.com>"
  :license "BSD 2 Clause"
  :serial t
  :depends-on (:alexandria)
  :components ((:file "package")
               (:file "core/utils")
               (:file "core/types")
               (:file "core/copy")
               (:file "core/typesystem")
               (:file "core/context")
               (:file "core/completeness")
               (:file "core/typed-expression")
               (:file "core/refs")
               (:file "core/unknowns")
               (:file "core/parameters")
               ;;
               (:file "core/common")
               (:file "core/constraints")
               (:file "core/ttype")
               (:file "core/generalize")
               ;;
               (:file "core/unify")
               (:file "core/ttype-param")
               (:file "core/check-and-infer")
               ;;
               (:file "core/print")))
