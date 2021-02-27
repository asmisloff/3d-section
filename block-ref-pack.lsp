(defun block-ref-pack  (/ dispatch)

  (defun dispatch  (method / make-3d-section flattened-copy)

    (defun flattened-copy  (bref deep / explode result)

      (defun explode  (bref)
	(mapcar	'vlax-vla-object->ename
		(vlax-safearray->list
		  (vlax-variant-value
		    (vla-explode
		      (vlax-ename->vla-object ent))))))

      (cond
	((protected? bref)
	 (setq result
		(cons
		  'SUBENTITIES-OF-PROTECTED-BLOCK
		  (flatten-list
		    (mapcar '(lambda (ent)
			       (g-flattened-copy ent (1+ deep)))
			    (explode bref))
		    nil)))
	 (if (> deep 0)
	   (entdel bref))
	 result)
	(T
	 (setq
	   result
	    (flatten-list
	      (mapcar '(lambda (ent)
			 (g-flattened-copy ent (1+ deep)))
		      (explode bref))
	      nil))
	 (if (> deep 0)
	   (entdel bref))
	 result)))

    (defun make-3d-section  (bref)
      (g-make-3d-section
	(g-flattened-copy bref 0)))

    (cond ((eq method 'make-3d-section) make-3d-section)
	  ((eq method 'flattened-copy) flattened-copy)))

  (cons "AcDbBlockReference" dispatch))

(defun protected?  (bref / ax-bref attributes tag text)
  (setq	ax-bref		    (vlax-ename->vla-object bref)
	attributes	    (vlax-variant-value
			      (vla-getattributes ax-bref))
	constant-attributes (vlax-variant-value
			      (vla-getconstantattributes
				ax-bref)))
  (if (safearray-empty? attributes)
    (setq attributes '())
    (setq attributes (vlax-safearray->list attributes)))
  (if (safearray-empty? constant-attributes)
    (setq constant-attributes '())
    (setq constant-attributes
	   (vlax-safearray->list
	     constant-attributes)))
  (vl-some
    '(lambda (att)
       (and (string=? (vla-get-tagstring att) "protected")
	    (vl-string-search
	      (strcase (vla-get-textstring att))
	      "YES")))
    (append attributes constant-attributes)))

(defun block-ref?  (ent)
  (= (vla-get-objectname (vlax-ename->vla-object ent))
     "AcDbBlockReference"))