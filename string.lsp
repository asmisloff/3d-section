(defun split-string  (str delimiter / res word)
  (setq	res  '()
	word '())
  (foreach ch  (vl-string->list str)
    (cond ((= ch (ascii delimiter))
	   (if word
	     (setq res	(cons (vl-list->string (reverse word)) res)
		   word	'())))
	  (T
	   (setq word (cons ch word)))))
  (if word
    (setq res (cons (vl-list->string (reverse word)) res)))
  (reverse res))

(defun string=?	 (s1 s2)
  (= (strcase s1) (strcase s2)))

(defun string->list  (s)
  (mapcar '(lambda (ch) (vl-list->string (list ch)))
	  (vl-string->list s)))