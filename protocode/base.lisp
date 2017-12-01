(in-package #:checkmate)

(defmacro checkmate (&environment env profile &body body)
  (declare (ignore profile))
  (let* ((static-env (macro-var-get 'static-env env)))
    (vbind (body new-bindings)
        (expand-static-code body (when static-env (bindings static-env)))
      (let ((new-env (env+ static-env new-bindings)))
        `(let-macro-var (static-env ,new-env)
           ,@body)))))

(defun expand-static-code (body bindings)
  (declare (ignore bindings))
  ;;
  ;; logic: if we recognise it, expand and walk. Else wrap it in a 'dynamically'
  ;;
  (values body '(a 1)))

(defun walk (code)
  (typecase code
    (list
     (let ((head (first code)))
       (typecase head
         (null nil)
         (symbol (cond
                   ((static-macro-name-p head)
                    (walk (expand-static-macro head code)))
                   ((checkmate-ignored-p head)
                    `(,head ,@(mapcar #'walk (rest code))))
                   (t `(dynamically (,head ,@(mapcar #'walk (rest code)))))))
         (otherwise code))))
    (otherwise code)))



(defmacro dynamically (&environment env &body body)
  (let ((static-env (macro-var-get 'static-env env)))
    (if static-env
        `(let-macro-var (static-env ,(env!))
           ,@body)
        `(progn ,@body))))

#+nil
(defun foo ()
  (checkmate ()
    10
    20
    (let ((x 10))
      (checkmate ()
        10
        (checkmate ()
          (dynamically
            1
            (dynamically
              (checkmate ()
                "foo"
                (checkmate ()
                  "again")))
            2
            3))
        (print x)))
    30))
