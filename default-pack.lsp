(defun default-pack  (/ dispatch)

  (defun dispatch  (method / make-3d-section flattened-copy)

    (defun flattened-copy  (ent deep / protected? explode result)
      (if (> deep 0)
	(entdel ent))
      nil)

    (defun make-3d-section  (ent)
      (if (> deep 0)
	(entdel ent))
      nil)

    (cond ((eq method 'make-3d-section) make-3d-section)
	  ((eq method 'flattened-copy) flattened-copy)))

  (cons 'DEFAULT dispatch))