(defun between-undo-marks (proc / doc err)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (vla-StartUndoMark doc)
  (setq err (vl-catch-all-apply (function proc) '()))
  (vla-EndUndoMark doc)
  (if (vl-catch-all-error-p err)
    (progn
      (print (vl-catch-all-error-message err))
      (print "Call undo."))))