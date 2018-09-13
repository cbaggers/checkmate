(in-package :checkmate)

;;------------------------------------------------------------

(defmacro ttype (designator)
  (designator->type designator))

(defun ttype-of (type-ref)
  (with-slots (target) type-ref
    (with-slots (spec) target
      (designator-from-type target))))

(defmacro define-ttype (designator
                        &body rest
                        &key where init custom-spec-data)
  (declare (ignore rest))
  (destructuring-bind (name . designator-args)
      (uiop:ensure-list designator)
    (destructuring-bind (req-args key-forms)
        (parse-ttype-lambda-list designator-args)
      (let* ((req-len (length req-args))
             (key-len (length key-forms))
             (args-len (+ req-len key-len))
             (key-args (mapcar #'first key-forms))
             (args (append req-args key-args))
             (where (loop :for arg :in args
                       :collect (or (find arg where :key #'first)
                                    (list arg 'ttype))))
             (arg-param-types (mapcar #'second where)))
        (alexandria:with-gensyms (gtype-spec)
          `(let ((init (or ,init #'identity)))
             (labels ((destructure-args (args)
                        (destructuring-bind (,@req-args &key ,@key-forms)
                            args
                          (list ,@req-args ,@key-args)))
                      (to-type (,gtype-spec
                                named-unknowns
                                constraints
                                args)
                        (assert (eq (slot-value ,gtype-spec 'name)
                                    ',name))
                        (let ((args (destructure-args args)))
                          (let* ((vals
                                  (make-array
                                   ,args-len
                                   :initial-contents
                                   (construct-designator-args ,gtype-spec
                                                              named-unknowns
                                                              constraints
                                                              args)))
                                 (is-complete
                                  (or (= (length vals) 0)
                                      (every (lambda (x)
                                               (complete-p (deref x)))
                                             vals))))
                            (take-ref
                             (make-instance 'user-ttype
                                            :spec ,gtype-spec
                                            :name ',name
                                            :arg-vals vals
                                            :known-complete is-complete))))))
               (register-type
                (let ((arg-param-specs
                       (make-array ,(length arg-param-types)
                                   :initial-contents
                                   (mapcar #'get-parameter-type-spec
                                           ',arg-param-types))))
                  (make-instance 'user-ttype-spec
                                 :name ',name
                                 :init init
                                 :arg-param-specs arg-param-specs
                                 :desig-to-type #'to-type
                                 :custom-data ',custom-spec-data)))
               ',name)))))))

(defun designator-from-type (type)
  (check-type type ttype)
  (flet ((desig (p)
           (if (typep p 'type-ref)
               (designator-from-type (deref p))
               (let ((naked-param (deref p))
                     (value (slot-value (deref p) 'value)))
                 (if (typep naked-param 'unknown-param)
                     (slot-value naked-param 'name)
                     value)))))
    (etypecase type
      (unknown
       (slot-value type 'name))
      (tfunction
       (with-slots (arg-types return-type) type
         `(function ,(mapcar #'ttype-of arg-types)
                    ,(ttype-of return-type))))
      (user-ttype
       (with-slots (name arg-vals) type
         (if (> (length arg-vals) 0)
             (cons name (map 'list #'desig arg-vals))
             name))))))

;;------------------------------------------------------------

(defun designator->type (type-designator)
  (internal-designator-to-type nil nil type-designator))

(defun internal-designator-to-type (named-unknowns constraints designator)
  (destructuring-bind (principle-name . args)
      (uiop:ensure-list designator)
    (case principle-name
      ;;
      ;; always use make-unknown
      (unknown
       (error "BUG: Attempt to make unknown type via designator"))
      ;;
      ;; non user type
      (function
       (assert (= (length designator) 3))
       (take-ref (make-instance
                  'tfunction
                  :arg-types (mapcar #'designator->type
                                     (second designator))
                  :return-type (designator->type
                                (third designator)))))
      ;;
      ;; is a user type or unknown
      (otherwise
       ;;
       ;; dont allow possibility of creating unknown if named-unknowns is nil
       ;; as this means you got here from a public facing method
       (if (and named-unknowns
                (unknown-designator-name-p designator))
           ;;
           ;; unknown
           (or (gethash designator named-unknowns)
               (setf (gethash designator named-unknowns)
                     (make-unknown (gethash designator constraints))))
           ;;
           ;; user type
           (let ((type-spec (get-user-type-spec designator)))
             ;; note: desig-to-type does return a ref
             (funcall (slot-value type-spec 'desig-to-type)
                      type-spec
                      named-unknowns
                      constraints
                      args)))))))

;;------------------------------------------------------------

;; {TODO} I'd like the user to have to specify the type of the designator
;;        argument. I think to start we will restrict it to types, symbols
;;        and numbers.
;; {TODO} where could be a a regular function rather than be generated by
;;        this macro
;;
;; hmm, we really need a way to define a type-system.
;; {TODO} ttype needs to have a load-form, which implies all
;;        elements in the designator need to have that too
;;------------------------------------------------------------
