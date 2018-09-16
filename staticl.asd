;;;; staticl.asd

(asdf:defsystem #:staticl
  :description "Foo"
  :author "Chris Bagley (Baggers) <chris.bagley@gmail.com>"
  :license "BSD 2 Clause"
  :serial t
  :depends-on (:checkmate)
  :components ((:file "test/staticl/package")
               (:file "test/staticl/impl")
               (:file "test/staticl/lang")))
