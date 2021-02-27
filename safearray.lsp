(defun safearray-empty?	 (arr)
  (= (vlax-safearray-get-u-bound arr 1) -1))

(defun list->safearray	(lst tp / arr)
  (setq arr (vlax-make-safearray tp (cons 0 (1- (length lst)))))
  (vlax-safearray-fill arr lst))