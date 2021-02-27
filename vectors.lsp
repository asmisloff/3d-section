(defun make-vector  (sp ep)
  (mapcar '- ep sp))

(defun dot-product  (v1 v2)
  (apply '+
	 (mapcar '* v1 v2)))

(defun vector-product  (a b / ax ay az bx by bz)
  (setq	ax (car a)
	ay (cadr a)
	az (caddr a)
	bx (car b)
	by (cadr b)
	bz (caddr b))
  (list
    (- (* ay bz)
       (* az by))
    (- (* az bx)
       (* ax bz))
    (- (* ax by)
       (* ay bx))))

(defun norm  (v)
  (sqrt
    (apply '+
	   (mapcar '(lambda (x) (expt x 2))
		   v))))

(defun normalize  (v / N)
  (setq N (norm v))
  (mapcar '(lambda (x) (/ x N)) v))