(in-package :checkmate)

(defmacro dbind (lambda-list expression &body body)
  `(destructuring-bind ,lambda-list ,expression ,@body))

(defmacro vbind (vars value-form &body body)
  ;; {TODO} handle declare forms properly. It is complicated
  ;;        as the declare has to be the first thing in the scope
  ;;        but the vars are now split across multiple binds
  (let* ((list? (mapcar #'listp vars))
         (mvb-vars (mapcar (lambda (v l?) (if l? (gensym) v)) vars list?))
         (d-vars (mapcar (lambda (v l?) (when l? v)) vars list?))
         (d-forms (mapcar (lambda (mvb d)
                            (when d `(dbind ,d ,mvb)))
                          mvb-vars d-vars))
         (d-forms (remove nil d-forms)))
    `(multiple-value-bind ,mvb-vars ,value-form
       ,@(reduce (lambda (accum x)
                   (list (append x accum)))
                 (cons body d-forms)))))

(defmacro vlist (value-form)
  `(multiple-value-list ,value-form))


(defun replace-all (seq old-elem new-elem)
  (loop for e in seq collect
       (if (eql e old-elem)
           new-elem
           e)))

(defun replace-at-n (seq n elem)
  (replace seq (list elem) :start1 n))

(defun last1 (list)
  (car (last list)))

(defun the-form-p (form)
  (eq (first form) 'the))
