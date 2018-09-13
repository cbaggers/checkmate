(in-package :checkmate)

;;------------------------------------------------------------

(defun unknown-designator-name-p (name)
  (and (symbolp name)
       (not (keywordp name))
       (let ((sname (symbol-name name)))
         (and (> (length sname ) 1)
              (char= (char sname 0) #\?)))))

(defun make-unknown-param ()
  (take-ref (make-instance 'unknown-param)))

(defun make-unknown (&optional constraints)
  (take-ref (make-naked-unknown constraints)))

(defun make-naked-unknown (constraints)
  (make-instance 'unknown :constraints constraints))

;;------------------------------------------------------------
