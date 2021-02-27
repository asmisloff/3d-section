(defun sset-foreach  (sset proc / i len)
  (setq	i   0
	len (sslength sset))
  (while (< i len)
    (proc (ssname sset i))
    (setq i (1+ i))))

(defun sset-map	 (sset proc / result)
  (setq	i   0
	len (sslength sset))
  (while (< i len)
    (setq result (cons (proc (ssname sset i)) result)
	  i	 (1+ i)))
  result)

(defun sset->list (sset)
  (sset-map sset (lambda (x) x)))

(defun list->sset (lst / ss)
  (setq ss (ssadd))
  (foreach ent lst
    (ssadd ent ss)))