(defun layer  (ent)
  (vla-get-layer (vlax-ename->vla-object ent)))

(defun copy  (ent)
  (vlax-vla-object->ename
    (vla-copy (vlax-ename->vla-object ent))))

(defun centroid  (sld)
      (vlax-safearray->list
	(vlax-variant-value
	  (vla-get-centroid (vlax-ename->vla-object sld)))))

(defun make-section	 (sld / sec)
      (setq sec	(vla-sectionsolid
		  (vlax-ename->vla-object sld)
		  (vlax-3d-point (trans '(0 0 0) 1 0))
		  (vlax-3d-point (trans '(1 0 0) 1 0))
		  (vlax-3d-point (trans '(0 1 0) 1 0))))
      (if sec
	(vlax-vla-object->ename sec)
	nil))