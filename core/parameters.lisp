(in-package :checkmate)

;;------------------------------------------------------------

(defun construct-designator-args (type-spec
                                  named-unknowns
                                  constraints
                                  vals)
  (with-slots (name arg-param-specs) type-spec
    (loop
       :for val :in vals
       :for param-spec :across arg-param-specs
       :for i :from 0
       :collect
       ;; dont need to handle function etc as that is covered by the to-param
       ;; of ttype parameters
       ;;
       ;; dont allow possibility of creating unknown if named-unknowns is nil
       ;; as this means you got here from a public facing method
         (if (and named-unknowns
                  (unknown-designator-name-p val))
             ;;
             ;; unknown
             ;;
             ;; may seem odd to use val here but ?x is also used for unknown
             ;; param designators
             (let ((already-seen (gethash val named-unknowns)))
               ;;
               ;; {TODO} clean up when combine unknowns
               (if (eq (slot-value param-spec 'name) 'ttype)
                   ;;
                   ;; type param
                   (progn
                     (when already-seen
                       (assert (typep already-seen 'type-ref) ()
                               "Argument ~a to ~a must be a type"
                               i name))
                     (or already-seen
                         (setf (gethash val named-unknowns)
                               (make-unknown (gethash val constraints)))))
                   ;;
                   ;; value param
                   (progn
                     (when already-seen
                       (assert (not (typep already-seen 'param-ref)) ()
                               "Argument ~a to ~a must be a type"
                               i name))
                     (or already-seen
                         (setf (gethash val named-unknowns)
                               (make-unknown-param))))))
             ;;
             ;; param type
             (funcall (slot-value param-spec 'to-param)
                      param-spec
                      val)))))

;;------------------------------------------------------------

(defun parse-ttype-lambda-list (lambda-list)
  (multiple-value-bind (required-parameters
                        optional-parameters
                        rest-parameters-name
                        keyword-parameters
                        has-allow-other-keys-p
                        aux-parameter)
      (alexandria:parse-ordinary-lambda-list lambda-list)
    (assert (not aux-parameter))
    (assert (not rest-parameters-name))
    (assert (not optional-parameters))
    (assert (not has-allow-other-keys-p))
    (assert (not (has-duplicates-p (append required-parameters
                                           keyword-parameters))))
    (list required-parameters
          (sort keyword-parameters #'string< :key #'first))))

;;------------------------------------------------------------

(defun make-parameter-type-spec (name valid-p equal)
  (assert (not (eq name 'ttype)))
  (assert (and (symbolp name) (not (keywordp name))))
  (let ((valid-p (or valid-p #'identity)))
    (labels ((to-param (spec val)
               (assert (eq (slot-value spec 'name) name))
               (assert (funcall valid-p val) ()
                       "~a is not a valid value to make a ~a type parameter"
                       val name)
               (take-ref
                (make-instance 'ttype-parameter
                               :name name
                               :spec spec
                               :value val))))
      (make-instance 'ttype-parameter-spec
                     :name name
                     :unify (lambda (a b mut-p)
                              (declare (ignore mut-p))
                              (funcall equal a b))
                     :to-param #'to-param))))

(defmacro define-parameter-type (name
                                 &body rest
                                 &key valid-p equal)
  (declare (ignore rest))
  (assert (not (eq name 'ttype)))
  (assert (and (symbolp name) (not (keywordp name))))
  `(progn
     (register-parameter-type
      (make-parameter-type-spec ',name ,valid-p ,equal))
     ',name))

;;------------------------------------------------------------
