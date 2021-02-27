(setq *table* nil)

(defun install-package	(pack)
  (setq *table* (cons (pack) *table*)))

(defun apply-generic  (method args / ent key dispatch proc)
  (setq	ent	 (car args)
	dispatch (search-package-for ent))
  (setq proc (dispatch method))
  (apply (function proc) args))

(defun search-package-for  (ent / object-name loop)
  (setq object-name (vla-get-ObjectName (vlax-ename->vla-object ent)))
  (defun loop  (tbl / key value key-type)
    (setq key	   (caar tbl)
	  value	   (cdar tbl)
	  key-type (type key))
    (cond ((null tbl)
	   (cdr (assoc 'default *table*)))
	  ((eq key-type 'STR)
	   (if (string=? object-name key)
	     value
	     (loop (cdr tbl))))
	  ((or (eq key-type 'SUBR)
	       (eq key-type 'USUBR))
	   (if (key ent)
	     value
	     (loop (cdr tbl))))
	  (T (loop (cdr tbl)))))
  (loop *table*))

;;;generic functions

(defun g-make-3d-section  (ent)
  (flatten-list
    (cond ((protected-list? ent)
	   (if (or (vl-some 'g-intersected-by-section-plane? (cdr ent))
		   (has-distinct-items?
		     (mapcar 'g-in-positive-side? (cdr ent)))
		   (vl-every '(lambda (item) (eq item t))
			     (mapcar 'g-in-positive-side? (cdr ent))))
	     (cdr ent)
	     (progn
	       (foreach	item  (cdr ent)
		 (entdel item))
	       nil)))
	  ((listp ent)
	   (mapcar 'g-make-3d-section ent))
	  (T (apply-generic 'make-3d-section (list ent))))
    nil))

(defun g-flattened-copy	 (ent deep)
  (apply-generic 'flattened-copy (list ent deep)))

(defun g-in-positive-side?  (ent)
  (apply-generic 'in-positive-side? (list ent)))

(defun g-intersected-by-section-plane?	(ent)
  (apply-generic 'intersected-by-section-plane? (list ent)))