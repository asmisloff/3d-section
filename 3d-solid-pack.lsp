(defun 3d-solid-pack  (/ dispatch)

  (defun dispatch  (method		    /
		    make-3d-section	    flattened-copy
		    in-positive-side?
		    intersected-by-section-plane?)

    (defun in-positive-side?  (sld)
      (> (dot-product
	   (make-vector
	     (trans (centroid sld) 0 1)
	     '(0 0 0))
	   '(0 0 1))
	 0))

    (defun intersected-by-section-plane?  (sld / tmp)
      (setq tmp (make-section sld))
      (if tmp
	(progn
	  (entdel tmp)
	  t)
	nil))

    (defun make-3d-section  (sld / slice sec hat)

      (defun slice  (sld / scrap)
	(setq scrap (vla-slicesolid
		      (vlax-ename->vla-object sld)
		      (vlax-3d-point (trans '(0 0 0) 1 0))
		      (vlax-3d-point (trans '(1 0 0) 1 0))
		      (vlax-3d-point (trans '(0 1 0) 1 0))
		      :vlax-true))
	(cond ((g-in-positive-side? sld)
	       (if scrap
		 (vla-delete scrap))
	       sld)
	      (T
	       (entdel sld)
	       (if scrap
		 (vlax-vla-object->ename scrap)
		 nil))))

      (if (setq sld (slice sld))
	(progn
	  (setq	sec (make-section sld)
		hat (hatch
		      sec
		      (get-pattern-by-layer (layer sld) *patterns*)))
	  (list sld sec hat))
	nil))

    (defun flattened-copy  (sld deep)
      (if (> deep 0)
	sld
	(copy sld)))

    (cond ((eq method 'make-3d-section) make-3d-section)
	  ((eq method 'flattened-copy) flattened-copy)
	  ((eq method 'in-positive-side?) in-positive-side?)
	  ((eq method 'intersected-by-section-plane?)
	   intersected-by-section-plane?)))

  (cons "AcDb3dSolid" dispatch))