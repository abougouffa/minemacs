;; -*- lexical-binding: t; -*-


(defalias 'string-split #'split-string)

(defmacro with-memoization (place &rest code)
  "Return the value of CODE and stash it in PLACE.
If PLACE's value is non-nil, then don't bother evaluating CODE
and return the value found in PLACE instead."
  (declare (indent 1) (debug (gv-place body)))
  (gv-letplace (getter setter) place
    `(or
      ,getter
      ,(macroexp-let2 nil val (macroexp-progn code)
        `(progn
           ,(funcall setter val)
           ,val)))))


(provide 'me-backports-29)
