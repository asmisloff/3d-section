(defun flatten-list  (lst		 unnest-protections?
		      /			 head
		      tail		 flatten-protected-list)
  (defun flatten-protected-list	 (lst remove-tag?)
    (if	remove-tag?
      (setq lst (cdr lst)))
    (flatten-list lst t))
  (setq	head (car lst)
	tail (cdr lst)
	remove-tag? nil)
  (cond	((null lst) nil)
	((protected-list? head)
	 ((if unnest-protections?
	    append
	    cons)
	   (flatten-protected-list head unnest-protections?)
	   (flatten-list tail unnest-protections?)))
	((listp head)
	 (append (flatten-list head unnest-protections?)
		 (flatten-list tail unnest-protections?)))
	(T
	 (cons head (flatten-list tail unnest-protections?)))))

(defun protected-list?	(lst)
  (and (listp lst)
       (eq (car lst) 'SUBENTITIES-OF-PROTECTED-BLOCK)))

(defun has-distinct-items?  (lst)
  (not
    (vl-every '(lambda (item) (eq item (car lst)))
	      lst)))

(defun list-search (lst item cmp / loop)
  (defun loop  (lst n)
    (cond ((null lst) nil)
	  ((cmp (car lst) item) n)
	  (T (loop (cdr lst) (1+ n)))))
  (loop lst 0))

(defun split-list  (lst delimiter cmp / left right n)
  (setq	n     (list-search lst delimiter cmp)
	left  '()
	right lst)
  (if n
    (while (> n 0)
      (setq left  (cons (car right) left)
	    right (cdr right)
	    n	  (1- n))))
  (list (reverse left) (car right) (cdr right)))