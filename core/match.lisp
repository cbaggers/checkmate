(in-package :checkmate)

;;------------------------------------------------------------

(defun bindingp (x)
  (and (symbolp x) (char= #\~ (char (symbol-name x) 0))))

(defun binding-var (x)
  (intern (subseq (symbol-name x) 1)
          (symbol-package x)))

;;------------------------------------------------------------

#+nil
(defun foo (blap)
  (match-ttype blap
    ((foo (bar ~a))
     (list "yay" a))
    (b8
     "boo")
    (otherwise
     "oh")))

;; {TODO} A way to improve this would be to group common sub-designators
;;        and use calling the local functions to navigate between these
;;        sub parts.

(defmacro match-ttype (form &body cases)
  (let* ((gform (gensym "ttype"))
         (gnames (loop :for i :below (length cases) :collect (gensym i)))
         (funcs (loop
                   :for (pattern . body) :in cases
                   :for (gname . rest-gnames) :on gnames
                   :collect
                     (if (eq pattern 'otherwise)
                         `(,gname () ,@body)
                         (list gname ()
                               `(when-ttype-matches
                                    ,gform ,pattern ,(first rest-gnames)
                                  ,@body))))))
    `(let ((,gform ,form))
       (labels ,funcs
         (,(first gnames))))))

(defmacro ematch-ttype (form &body cases)
  (let ((patterns (remove 'otherwise (mapcar #'first cases))))
    `(match-ttype
         ,form ,@cases
         (otherwise (match-err ,form ,patterns)))))

(defmacro when-ttype-matches (var pattern next &body body)
  (expand-ttype-pattern var pattern body next t))

(defmacro match-err (form patterns)
  `(error
    ,(format nil
             "ematch-type: The form ~a did not match any of:~{~%- ~a~}"
             form patterns)))

(defmacro dbind-ttype (pattern form &body body)
  (let* ((gform (gensym "ttype")))
    `(let ((,gform ,form))
       ,(expand-ttype-pattern gform pattern body nil nil))))

#+nil
(defun goo (jam)
  (dbind-ttype (function (~a) i8) jam
    a))

;;------------------------------------------------------------

(defun expand-ttype-pattern (type-ref pattern body on-fail use-tests)
  (let* ((symb (gensym "temp"))
         (pairs (list (list symb pattern)))
         (res nil))
    (loop
       :while pairs
       :for (symb pattern) := (pop pairs)
       :do (let* ((r (multiple-value-list
                      (dispatch-expand-pattern symb pattern)))
                  (next (last1 r)))
             (push r res)
             (setf pairs (append next pairs))))
    (let ((body
           (reduce
            (lambda (wip set)
              (destructuring-bind (self-test
                                   guarenteed-bindings
                                   tests
                                   resulting-bindings
                                   next)
                  set
                (declare (ignore next))
                (let* ((rbind
                        (if resulting-bindings
                            `((let ,resulting-bindings
                                (declare
                                 (ignorable
                                  ,@(remove-if
                                     #'symbol-package
                                     (mapcar #'car resulting-bindings))))
                                ,@wip))
                            wip))
                       (test (if use-tests
                                 `(if (and ,@tests)
                                      (progn ,@rbind)
                                      ,(when on-fail `(,on-fail)))
                                 `(progn ,@rbind)))
                       (gbind
                        (if guarenteed-bindings
                            `(let ,guarenteed-bindings
                               (declare
                                (ignorable
                                 ,@(remove-if
                                    #'symbol-package
                                    (mapcar #'car guarenteed-bindings))))
                               ,test)
                            test)))
                  (if (or (eq self-test t) (not use-tests))
                      `(,gbind)
                      `((if ,self-test
                            ,gbind
                            ,(when on-fail `(,on-fail))))))))
            res
            :initial-value body)))
      `(let ((,symb (deref ,type-ref)))
         (declare (ignorable ,symb))
         ,@body))))

(defun dispatch-expand-pattern (symb pattern)
  (assert (not (bindingp pattern)))
  (cond
    ((and (listp pattern) (eq (first pattern) 'function))
     (expand-function-pattern symb pattern))
    ((atom pattern)
     (expand-explicit-atom-pattern symb pattern))
    (t
     (expand-form-pattern symb pattern))))

(defun expand-function-pattern (symb pattern)
  (let ((g-arg-types (gensym "arg-types"))
        (g-ret-type (gensym "ret-type"))
        (cbindings nil))
    (destructuring-bind (arg-patterns ret-pattern) (rest pattern)
      (let ((next
             (append
              (when (listp arg-patterns)
                (loop
                   :for arg :in arg-patterns
                   :for i :from 0
                   :unless (bindingp arg)
                   :collect
                     (let* ((asymb (gensym (format nil "arg~a" i)))
                            (b `(,asymb (deref (aref ,g-arg-types ,i)))))
                       (push b cbindings)
                       (list asymb arg))))
              (unless (bindingp ret-pattern)
                (let* ((rsymb (gensym "ret"))
                       (b `(,rsymb (deref ,g-ret-type))))
                  (push b cbindings)
                  `((,rsymb ,ret-pattern)))))))
        (values
         ;; self test
         `(typep ,symb 'tfunction)
         ;; guarenteed bindings
         `((,g-arg-types (slot-value ,symb 'arg-types))
           (,g-ret-type (slot-value ,symb 'return-type)))
         ;; tests
         (when (listp arg-patterns)
           `((= (length ,g-arg-types) ,(length arg-patterns))))
         ;; resulting bindings
         `(,@(if (listp arg-patterns)
                 (loop
                    :for arg :in arg-patterns
                    :for i :from 0
                    :when (and (bindingp arg) (not (string= "~" arg)))
                    :collect `(,(binding-var arg)
                                (aref ,g-arg-types ,i)))
                 (when (not (string= "~" arg-patterns))
                   `((,(binding-var arg-patterns)
                       (copy-seq ,g-arg-types)))))
             ,@(when (and (bindingp ret-pattern)
                          (not (string= "~" ret-pattern)))
                 `((,(binding-var ret-pattern)
                     ,g-ret-type)))
             ,@cbindings)
         ;; next to expand
         next)))))

(defun expand-explicit-atom-pattern (symb pattern)
  (values
   ;; self test
   t
   ;; guarenteed bindings
   nil
   ;; tests
   `((typep ,symb 'user-ttype)
     (eq (slot-value ,symb 'name) ',pattern))
   ;; resulting bindings
   nil
   ;; next to expand
   nil))

(defun expand-form-pattern (symb pattern)
  (let ((g-arg-vals (gensym "arg-vals"))
        (cbindings nil))
    (destructuring-bind (pname . arg-patterns) pattern
      (let ((next
             (loop
                :for arg :in arg-patterns
                :for i :from 0
                :unless (bindingp arg)
                :collect
                  (let* ((asymb (gensym (format nil "param~a" i)))
                         (b `(,asymb (deref (aref ,g-arg-vals ,i)))))
                    (push b cbindings)
                    (list asymb arg)))))
        (values
         ;; self test
         `(and (typep ,symb 'user-ttype)
               (eq (slot-value ,symb 'name) ',pname))
         ;; guarenteed bindings
         `((,g-arg-vals (slot-value ,symb 'arg-vals)))
         ;; tests
         `((= (length ,g-arg-vals) ,(length arg-patterns)))
         ;; resulting bindings
         `(,@(loop
                :for arg :in arg-patterns
                :for i :from 0
                :when (bindingp arg)
                :collect `(,(binding-var arg)
                            (aref ,g-arg-vals ,i)))
             ,@cbindings)
         ;; next to expand
         next)))))

;;------------------------------------------------------------
