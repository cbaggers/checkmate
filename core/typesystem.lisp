(in-package :checkmate)

;;------------------------------------------------------------

(defvar *registered-user-types* (make-hash-table :test #'eq))
(defvar *registered-parameter-types* (make-hash-table :test #'eq))
(defvar *registered-constraints* (make-hash-table :test #'eq))

(defvar *last-dropped-type* nil)
(defvar *last-dropped-constraint* nil)
(defvar *last-dropped-parameter-type* nil)

(defgeneric register-type (type-spec)
  (:method (spec)
    ;; this ↓ is just for debugging
    (setf *last-dropped-type* spec)
    (warn "register-type is not implemented")))

(defgeneric register-constraint (type-spec)
  (:method (spec)
    ;; this ↓ is just for debugging
    (setf *last-dropped-constraint* spec)
    (warn "register-type is not implemented")))

(defgeneric register-parameter-type (parameter-type-spec)
  (:method (spec)
    ;; this ↓ is just for debugging
    (setf *last-dropped-parameter-type* spec)
    (warn "register-parameter-type is not implemented")))

(defmethod register-type (spec)
  (with-slots (name) spec
    (format t "~%;; Registered type ~a" name)
    (setf (gethash name *registered-user-types*) spec)))

(defmethod register-constraint (spec)
  (with-slots (name) spec
    (format t "~%;; Registered constraint ~a" name)
    (setf (gethash name *registered-constraints*) spec)))

(defmethod register-parameter-type (spec)
  (with-slots (name) spec
    (format t "~%;; Registered param type ~a" name)
    (setf (gethash name *registered-parameter-types*) spec)))

;;------------------------------------------------------------

(defun get-user-type-spec (designator)
  (let ((principle-name (first (alexandria:ensure-list designator))))
    (or (gethash principle-name *registered-user-types*)
        (error "Could not identify type for designator: ~a"
               designator))))

(defun get-parameter-type-spec (name)
  (or (gethash name *registered-parameter-types*)
      (error
       "define-ttype: ~a is not valid designator arg type.~%valid:~a"
       name (alexandria:hash-table-keys *registered-parameter-types*))))

(defun get-constraint-spec (designator)
  (let ((principle-name (first (alexandria:ensure-list designator))))
    (or (gethash principle-name
                 *registered-constraints*)
        (error "Could not identify constraint for designator: ~a"
               designator))))

;;------------------------------------------------------------
