(in-package :checkmate)

;;------------------------------------------------------------

(defun expand-code (code)
  (typecase code
    (list (expand-form code))
    (t code)))

(defun expand-form (code)
  (let ((head (first code)))
    (if (member head *supported-special-operators*)
        (expand-special-form code)
        (expand-standard-form code))))

;;------------------------------------------------------------


(defun expand-standard-form (code)
  (dbind (name . args) code
    (cond
      ((static-macro-name-p name)
       (expand-code (expand-static-macro name code)))
      (t `(funcall #',name ,@(mapcar #'expand-code args))))))


;;------------------------------------------------------------

(defun expand-special-form (code)
  (let ((head (first code)))
    (ecase head
      ;; quote
      (quote code)
      ;; function
      (function code)
      ;; if
      (if (dbind (ts tn el) (rest code)
            `(if ,(expand-code ts)
                 ,(expand-code tn)
                 ,(expand-code el))))
      ;; let
      (let (labels ((expand-bind (b)
                      (dbind (n f) b
                        `(,n ,(expand-code f)))))
             (dbind (bindings . body) (rest code)
               `(let ,(mapcar #'expand-bind bindings)
                  ,@(mapcar #'expand-code body)))))
      ;; progn
      (progn `(progn
                ,@(mapcar #'expand-code (rest code))))
      ;; export
      ;; block
      ;; catch
      ;; eval-when
      ;; flet
      ;; function
      ;; go
      ;; labels
      ;; load-time-value
      ;; locally
      ;; macrolet
      ;; multiple-value-call
      ;; multiple-value-prog1
      ;; progv
      ;; return-from
      ;; setq
      ;; symbol-macrolet
      ;; tagbody
      ;; the
      ;; throw
      ;; unwind-protect
      )))

(defparameter *supported-special-operators*
  '(quote let if progn))

(defparameter *special-operator*
  '(export
    block
    catch
    eval-when
    flet
    function
    go
    if
    labels
    let
    let*
    load-time-value
    locally
    macrolet
    multiple-value-call
    multiple-value-prog1
    progn
    progv
    quote
    return-from
    setq
    symbol-macrolet
    tagbody
    the
    throw
    unwind-protect))
