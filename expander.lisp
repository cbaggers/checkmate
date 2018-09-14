(in-package :checkmate.internals)
(in-readtable :checkmate.readtable)

(defun expand-fully (body)
  body)

(defun extract-untyped-lambda-list (typed-lambda-list)
  (values (parse-defn-args typed-lambda-list nil)))

(defun parse-defn-args (typed-args result-types)
  (let ((seen-&key nil)
        (seen-&rest nil)
        (seen-&optional nil)
        (f-args nil)
        (f-sigs nil)
        (f-decls nil))
    (labels ((kwd (x) (intern (symbol-name x) :keyword)))
      (loop :for x :in typed-args :do
         (destructuring-bind (name &optional (type t) opt-val) (ensure-list x)
           (cond
             ((string= name "&KEY")
              (when seen-&optional
                (error "can't combine &rest/&optional/&key in same defn"))
              (setf seen-&key t)
              (push name f-args)
              (push name f-sigs))
             ((string= name "&REST")
              (when seen-&optional
                (error "can't combine &rest/&optional/&key in same defn"))
              (setf seen-&rest t)
              (push name f-args)
              (push name f-sigs))
             ((string= name "&OPTIONAL")
              (when seen-&rest
                (error "can't combine &rest/&optional/&key in same defn"))
              (setf seen-&optional t)
              (push name f-args)
              (push name f-sigs))
             ((char= #\& (char (symbol-name name) 0))
              (error "~a not valid in defn forms" name))
             (t
              (let ((decl-type (cond
                                 (seen-&key `(or ,type null))
                                 (seen-&optional `(or ,type null))
                                 (t type)))
                    (f-sig (cond
                             (seen-&key `(,(kwd name) ,type))
                             (seen-&optional `(or ,type null))
                             (t type)))
                    (f-arg (cond
                             (seen-&key `(,name ,opt-val))
                             (seen-&optional `(,name ,opt-val))
                             (t name))))
                (unless seen-&rest
                  (push `(type ,decl-type ,name) f-decls))
                (push f-sig f-sigs)
                (push f-arg f-args)))))))
    ;;
    (values (reverse f-args)
            (if result-types
                `(function ,(reverse f-sigs) ,result-types)
                :unknown)
            (reverse f-decls))))
