(in-package :checkmate)

(deferror cannot-unify-types () (type-a type-b)
    "
CHECKMATE: Could not unify ~a and ~a"
  type-a type-b)

(deferror cannot-unify-params () (a-name a-val b-name b-val)
    "
CHECKMATE: Could not unify params:
~a: ~a
~a: ~a"
  a-name a-val b-name b-val)

(deferror failed-to-satisfy-constraints () (type-ref failed)
    "
Type ~a failed to satisfy the following constraints:
~{~a~}"
    type-ref failed)
