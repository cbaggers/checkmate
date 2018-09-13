;; (defmethod no-applicable-method ((method (eql #'foo)) &rest args)
;;   (format t "no applicable method for (foo ~{~s~^ ~})~%" args))

;; This first version will have no multiple value return and
;; will use void as it's (values) type.

;;------------------------------------------------------------
;; TYPES

;; You can have data-types, traits, 'and' types & 'or' types
;; 'or' types can contain traits and data-types
;; 'and' types can only contain traits
;; functions can be polymorphic
;; functions can only have one signature

;; We can be polymorphic over trait argument pretty easily by
;; just having an underspecified trait in a function
;; hmm
;; do we want that?
;;
;; (defn foo ((i integer) (c collection))
;;   ..)
;;
;; -v-
;;
;; (defn foo ((i integer) (c (collection ?)))
;;   ..)
;;
;; there is at least noise in the first one, but it is down to the
;; user to know that it is a trait rather than a data-type. One nice
;; thing is that it does minimize paren overload in cases where it's
;; going to be infered anyway
;; I think I like it.
;;
;; Oh yeah, back to the planning.. I was wondering about non-type args
;; to a trait (or data-type? hmm not sure what that would mean)
;;
;; The classic example is (array single-float (5)) which stores the size
;; of the array in the type. We can either dictate the inference for certain
;; types or open this up to the user.
;;
;; What do we need to infer the length? Well lets take this func
;;
;; (defn foo ((arr (array single-float (?))))
;;   (setf (aref arr 5) 5)
;;   (setf (aref arr 10) 10)
;;   arr)
;;
;; the array needs to have a length of at least 10 otherwise this code will
;; fail. We cant infer length from first form as otherwise second will fail
;; or the second as any length >= 10 is valid.
;; Also, when constructed we cant allow non integer lengths.. maybe this is
;; controlled by the constructor function itself, you cant pass a float to
;; the :count arg so its no worry.
;;
;; For the array case we need a range [10 -> ∞]
;; what does that mean in practice?
;; array could have min-length & max-length slots, there could be an 'infer'
;; macro defined for (aref array (constant ?)) which would then set min-length
;; to (max min-length 10) or whatever.
;;
;; infer-macros risk clashes, do they just stack? What if they come up with
;; different inferences for the same piece of data? Maybe we limit it to only 1
;; per signature... we need to detect clashing signatures in that case.. which
;; may be interesting anyway.
;;
;; I think infer-macros will be like compiler-macros in that they run after
;; standard inference and are used to augment the type, they will handle the
;; non-type arguments
;;
;; (define-trait (array ?elem !) ()
;;   (aref array integer)
;;   (setf (aref (array ?elem !) integer) ?elem))
;;
;; where ? indicates a type argument and ! indicates a dependent argument.
;; Dependent args will always be handled by user defined methods
;;
;; This brings up the issue of unification. How do you unify these user
;; dependent arguments? Gonna have to be a user api for this.
;;
;; The good news though is we can get going without this and add them later,
;; we just need to be mindful of what we are doing when writing the base api.
;;
;; Actually we could put in placeholders so we can have sized arrays, the
;; default unify would just be #'eql. There would be no inference for these
;; args so the funcs would be polymorphic over length, which works for aref and
;; such. This lets us have funcs taking sized arrays too. Just gotta be careful
;; not to paint ourselves into a corner.
;;
;; Regarding the ?0 notation, maybe we could use (type 0) and thus some reader
;; macro ala quote. Oriignally thought #t but this sucks e.g. #telem so #? or
;; something? egh, it's hard as reader macros are not the most loved things.
;; Also we need something else for dependent args..hmm. Lets revisit this after
;; working with ?foo and !bar for a bit.
