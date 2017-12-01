(in-package :checkmate)

;;--------------------------------------------------
;; Magic by stassats

(defun macro-var-get (name env)
  (getf (macroexpand-1 '(declaration-macro) env) name))

(defmacro declaration-macro () nil)

(defmacro let-macro-var ((name value) &body body &environment env)
  `(macrolet ((declaration-macro ()
                '(,name ,value ,@(macroexpand-1 '(declaration-macro) env))))
     ,@body))

;;--------------------------------------------------

(defclass static-env ()
  ((bindings :initform nil :initarg :bindings)
   (parent :initform nil :initarg :parent :reader parent)))

(defun env! (&optional bindings)
  (make-instance 'static-env :bindings bindings))

(defun env+ (env bindings)
  (make-instance 'static-env :bindings bindings :parent env))

(defmethod make-load-form ((s-env static-env) &optional environment)
  (declare (ignore environment))
  (with-slots (bindings) s-env
    `(env! ,@bindings)))

(defmethod bindings ((env static-env))
  (with-slots (bindings parent) env
    (append bindings (when parent (bindings parent)))))
