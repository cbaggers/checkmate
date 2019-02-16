(in-package :checkmate)

;; {TODO} disallow names to start with ~

;;------------------------------------------------------------

(defun construct-designator-args (context
                                  type-spec
                                  named-unknowns
                                  constraints
                                  vals)
  (with-slots (name arg-param-specs) type-spec
    (assert (= (length arg-param-specs) (length vals)) ()
            "Not enough values in ~a to satify ~a"
            (cons name vals)
            (cons name (map 'list (lambda (x) (slot-value x 'name))
                            arg-param-specs)))
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
             (to-param context
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
    (assert (not keyword-parameters))
    (assert (not (has-duplicates-p required-parameters)))
    required-parameters))

;;------------------------------------------------------------

(defun ttype-designator-to-param (context val)
  (designator->type context val))

(defun late-initialize-param-spec (spec)
  (unless (slot-boundp spec 'equal)
    (setf (slot-value spec 'equal)
          (when (slot-value spec 'equal-name)
            (symbol-function (slot-value spec 'equal-name)))))
  (unless (slot-boundp spec 'valid-p)
    (setf (slot-value spec 'valid-p)
          (when (slot-value spec 'valid-p-name)
            (symbol-function (slot-value spec 'valid-p-name))))))

(defun to-param (context spec val)
  (late-initialize-param-spec spec)
  (let ((name (slot-value spec 'name)))
    (if (eq name 'ttype)
        (ttype-designator-to-param context val)
        (let ((valid-p (slot-value spec 'valid-p)))
          (assert (funcall valid-p val) ()
                  "~a is not a valid value to make a ~a type parameter"
                  val name)
          (take-ref
           (make-instance 'ttype-parameter
                          :name name
                          :spec spec
                          :value val))))))

(defun make-parameter-spec (type-system name valid-p equal)
  (declare (ignore type-system))
  (assert (not (eq name 'ttype)))
  (assert (and (symbolp name) (not (keywordp name))))
  (assert (symbolp valid-p))
  (assert (symbolp equal))
  ;; {TODO} remove unify and replace with hardcoding in unify.lisp/87
  (make-instance 'ttype-parameter-spec
                 :name name
                 :equal-name equal
                 :valid-p-name (or valid-p 'identity)))

;;------------------------------------------------------------
