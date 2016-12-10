(in-package :checkmate)
(in-readtable :fn.reader)

;;------------------------------------------------------------

(defclass fact () ())

;;------------------------------------------------------------

(defclass top (fact) ())
(defclass bottom (fact) ())

(defvar top (make-instance 'top))
(defvar bottom (make-instance 'bottom))

;;------------------------------------------------------------

(defclass check-environment ()
  ((parent :initform nil :initarg :parent)
   (bindings :initform nil :initarg :bindings)))

(defclass var-fact-binding ()
  ((name :initform nil :initarg :name :reader name)
   (fact :initform nil :initarg :fact :reader fact)))

(defun env-bind (env bindings)
  (assert (every λ(typep _ 'var-fact-binding) bindings))
  (make-instance (type-of env)
                 :parent env
                 :bindings bindings))

(defun get-binding (name env)
  (with-slots (bindings parent) env
    (or (find name bindings :key #'name)
        (when parent
          (get-binding name parent)))))

(defvar *env-roots* (make-hash-table))

;;------------------------------------------------------------

(defun fact-expand (code &optional env)
  (let ((env (etypecase env
               (symbol (or (gethash env *env-roots*)
                           (error "~a not a known checker" env)))
               (check-environment env))))
    (typecase code
      (symbol (fact-expand-symbol code env))
      (list (fact-expand-form code env))
      (otherwise `(the ,(%infer code env) ,code)))))

;;------------------------------------------------------------
;; vars

(defun fact-expand-symbol (code env)
  (let ((binding (get-binding code env)))
    (assert binding () "Cant find ~a" code)
    `(the ,(fact binding) ,code)))

;;------------------------------------------------------------
;; special forms

(defun fact-expand-form (code env)
  (dbind (name . args) code
    (ecase name
      (let (fact-expand-let-form args env))
      (progn (fact-expand-progn-form args env))
      (quote (fact-expand-quote-form args env))
      (funcall (fact-expand-funcall-form args env))
      (function (fact-expand-function-form args env)))))

(defun fact-expand-quote-form (args env)
  (assert (= (length args) 1))
  (let ((fact (%infer (first args) env)))
    `(quote (the ,fact ,@args))))

(defun %fact-expand-progn-form (args env)
  (mapcar λ(fact-expand _ env) args))

(defun fact-expand-progn-form (args env)
  (let* ((code (%fact-expand-progn-form args env))
         (last-form (last1 code)))
    (assert (the-form-p (first last-form)))
    `(the ,(second last-form) (progn ,@code))))

(defun fact-expand-let-form (args env)
  (labels ((fact-expand-binding (b)
             (dbind (n f) b
               (make-instance
                'var-fact-binding
                :name n :fact (%infer f env)))))
    (dbind (bindings . body) args
      (let* ((var-fact-bindings (mapcar #'fact-expand-binding bindings))
             (lex-env (env-bind env var-fact-bindings))
             (body (%fact-expand-progn-form body lex-env))
             (progn-fact (second (last1 body))))
        `(the ,progn-fact
              (let ,(mapcar λ`(,(name _) (the ,(fact _) ,(second _1)))
                            var-fact-bindings
                            bindings)
                ,@body))))))

(defun fact-expand-function-form (code env)
  ;; should check global & local envs
  (declare (ignore env))
  (assert (= (length code) 1))
  (let ((function (first code)))
    `(the ,(make-instance 'fact) (function ,function))))

(defun fact-expand-funcall-form (code env)
  (dbind (func . args) (mapcar λ(fact-expand _ env) code)
    (let* ((func-fact (second func))
           (arg-facts (mapcar #'second args))
           (return-fact (check-call func-fact arg-facts env)))
      `(the ,return-fact
            (funcall ,func ,@args)))))

;;------------------------------------------------------------
;; methods

(defgeneric %fact-for (env &key))

(defun %infer (code env)
  (apply #'%fact-for env (multiple-value-list (infer code env))))

(defmethod infer (code env)
  (error "No infer defined for ~a in ~a"
         code (type-of env)))

(defmethod check-call (func-fact arg-facts env)
  (error "No check-call defined for ~a with ~a in ~a"
         func-fact arg-facts (type-of env)))

;;------------------------------------------------------------
