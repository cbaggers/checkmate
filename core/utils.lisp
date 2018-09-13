(in-package :checkmate)

;;------------------------------------------------------------

(defun has-duplicates-p (list)
  (loop :for (val . rest) :on list
     :when (find val rest)
     :do (return t)))

;;------------------------------------------------------------
