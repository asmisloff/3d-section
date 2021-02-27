(defun hatch  (reg pattern / hat)
  (defun eval-scale  (reg)
    (* 20
       (sqrt
	 (vla-get-area
	   (vlax-ename->vla-object reg)))
       1e-3))
  (if reg
    (progn
      (command "_-hatch"
	       "_S"
	       reg
	       ""
	       "P"
	       pattern
	       (eval-scale reg)
	       0.0
	       "")
      (setq hat (entlast))
      (vla-put-color (vlax-ename->vla-object hat) 8)
      hat)
    nil))

(defun read-patterns-from-file	(path / res str)
  (setq	file (open (findfile path) "r")
	res  '())
  (while (setq str (read-line file))
    (setq res (cons (split-string str "\t") res)))
  (close file)
  (reverse res))

(defun get-pattern-by-layer (layer patterns / curr)
  (setq curr (car patterns))
  (cond ((null patterns) "ANSI31")
	((wcmatch (strcase layer T)
		  (strcase (car curr) T))
	 (cadr curr))
	(T (get-pattern-by-layer layer (cdr patterns)))))