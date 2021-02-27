(defun xref-pack  (/ dispatch)

  (defun dispatch  (method / flattened-copy)

    (defun flattened-copy
	   (xref deep / collection->safearray explode result)

      (defun collection->safearray  (col / len arr i)
	(setq len (vla-get-Count col)
	      arr (vlax-make-safearray
		    vlax-vbObject
		    (cons 0 (1- len)))
	      i	  0)
	(while (< i len)
	  (vlax-safearray-put-element arr i (vla-Item col i))
	  (setq i (1+ i)))
	arr)

      (defun explode  (xref / t-matrix docs xref-doc ms ents layer-name)
	(setq t-matrix (vlax-tmatrix (sbt:get-tmatrix xref))
	      docs     (vla-get-documents (vlax-get-acad-object))
	      xref-doc (vla-open
			 docs
			 (vla-get-path (vlax-ename->vla-object xref)))
	      ms       (vla-get-ModelSpace
			 (vla-get-ActiveDocument (vlax-get-acad-object)))
	      ents     (vlax-safearray->list
			 (vlax-variant-value
			   (vla-CopyObjects
			     xref-doc
			     (collection->safearray
			       (vla-get-ModelSpace xref-doc))
			     (vlax-make-variant ms vlax-vbObject)))))
	(vla-close xref-doc :vlax-false)
	(foreach ent  ents
	  (vla-TransformBy ent t-matrix)
	  (setq layer-name (vla-get-layer ent)))
	(mapcar 'vlax-vla-object->ename ents))

;;;      (cond
;;;	((protected? xref)
;;;	 (setq result
;;;		(cons
;;;		  'SUBENTITIES-OF-PROTECTED-BLOCK
;;;		  (flatten-list
;;;		    (mapcar '(lambda (ent)
;;;			       (g-flattened-copy ent (1+ deep)))
;;;			    (explode xref))
;;;		    nil)))
;;;	 (if (> deep 0)
;;;	   (entdel xref))
;;;	 result)
;;;	(T
;;;	 (setq
;;;	   result
;;;	    (flatten-list
;;;	      (mapcar '(lambda (ent)
;;;			 (g-flattened-copy ent (1+ deep)))
;;;		      (explode xref))
;;;	      nil))
;;;	 (if (> deep 0)
;;;	   (entdel xref))
;;;	 result)))
      (setq result (flatten-list
		     (mapcar '(lambda (ent)
				(g-flattened-copy ent (1+ deep)))
			     (explode xref))
		     nil))
      (if (protected? xref)
	(setq result (cons 'SUBENTITIES-OF-PROTECTED-BLOCK result)))
      (if (> deep 0)
	(entdel xref))
      result)

    (defun make-3d-section  (xref)
      (g-make-3d-section (g-flattened-copy xref 0)))

    (cond ((eq method 'make-3d-section) make-3d-section)
	  ((eq method 'flattened-copy) flattened-copy)))

  (cons xref? dispatch))


(defun get-block-by-name  (name / result)
  (setq result nil)
  (vlax-for block  (vla-get-blocks
		     (vla-get-ActiveDocument (vlax-get-acad-object)))
    (if	(string=? name (vla-get-Name block))
      (setq result block)))
  (if result
    result
    nil))

(defun xref?  (ent)
  (and (block-ref? ent)
       (eq (vla-get-IsXRef
	     (get-block-by-name (vla-get-Name (vlax-ename->vla-object ent))))
	   :vlax-true)))



