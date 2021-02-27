(defun make-block  (ents name)
  (command "_ucs" "_w")
  (command "-block"
	   name
	   (getpoint "Insertion point: ")
	   (list->sset ents)
	   ""))

(defun generate-name  (/ existing-names names i)

  (defun existing-names	 (/ result)
    (setq result '())
    (vlax-for block  (vla-get-blocks
		       (vla-get-activedocument (vlax-get-acad-object)))
      (setq result (cons (vla-get-name block) result)))
    (reverse result))

  (setq	i     1
	names (existing-names))
  (while (vl-member-if
	   '(lambda (x) (string=? x (strcat "#" (itoa i))))
	   names)
    (setq i (1+ i)))
  (strcat "#" (itoa i)))

(defun c:make-3d-section  (/ cmdecho ins-point)
  (setq *patterns* (read-patterns-from-file "z:/project/3d-section/patterns.sec"))
  (setq cmdecho (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (between-undo-marks
    (lambda (/ result auto-name user-name)
      (setq result    (flatten-list
			(mapcar	'g-make-3d-section
				(sset->list (ssget)))
			nil)
	    auto-name (generate-name)
	    user-name (getstring (strcat "Name for block <"
					 auto-name
					 ">:
					 ")))

      ;(print user-name)
      (make-block
	result
	(if (string=? "" user-name)
	  auto-name
	  user-name))))
  (setvar "cmdecho" cmdecho)
  (princ "\nBlock saved. Call _insert.")
  (princ))