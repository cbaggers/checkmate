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
  ((profile :initform nil :initarg :profile)
   (parent :initform nil :initarg :parent)
   (bindings :initform nil :initarg :bindings)))

(defclass var-fact-binding ()
  ((name :initform nil :initarg :name :reader name)
   (fact :initform nil :initarg :fact :reader fact)))

(defun env-bind (env bindings)
  (assert (every λ(typep _ 'var-fact-binding) bindings))
  (make-instance 'check-environment
                 :profile (slot-value env 'profile)
                 :parent env
                 :bindings bindings))

;;------------------------------------------------------------

(defun fact-expand (code env)
  (typecase code
    (symbol (fact-expand-symbol code env))
    (list (fact-expand-form code env))
    (otherwise `(the ,(infer code env) ,code))))

;;------------------------------------------------------------

(defun fact-expand-symbol (code env)
  `(the ,(infer code env) ,code))

;;------------------------------------------------------------

(defun fact-expand-form (code env)
  (dbind (name . args) code
    (ecase name
      (let (fact-expand-let-form args env))
      (progn (fact-expand-progn-form args env))
      (quote (fact-expand-quote-form args env))
      (funcall (fact-expand-funcall-form args env)))))

(defun fact-expand-quote-form (args env)
  (assert (= (length args) 1))
  (let ((fact (infer (first args) env)))
    `(quote (the ,fact ,@args))))

(defun %fact-expand-progn-form (args env)
  (mapcar λ(fact-expand _ env) args))

(defun fact-expand-progn-form (args env)
  (let* ((code (%fact-expand-progn-form args env))
         (last-form (last1 code)))
    (assert (eq 'the (first last-form)))
    `(the ,(second last-form) (progn ,@code))))

(defun fact-expand-let-form (args env)
  (labels ((fact-expand-binding (b)
             (dbind (n f) b
               (make-instance
                'var-fact-binding
                :name n :fact (infer f env)))))
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

(defun fact-expand-funcall-form (code env)
  (dbind (func . args) code
    (let* ((func-fact (infer func env))
           (arg-facts (mapcar λ(infer _ env) args))
           (return-fact (fact-expand-call func-fact arg-facts env)))
      `(the ,return-fact
            (funcall (the ,func-fact ,func)
                     ,@(mapcar λ`(the ,_ ,_1)
                               arg-facts
                               (rest code)))))))

;;------------------------------------------------------------

(defmethod infer (code env)
  (make-instance 'fact))

(defmethod fact-expand-call (func-fact arg-facts env)
  (make-instance 'fact))

;;------------------------------------------------------------
