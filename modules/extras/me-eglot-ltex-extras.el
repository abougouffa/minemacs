;; -*- lexical-binding: t; -*-

(defvar eglot-ltex-dictionary
  (+deserialize-sym
   'eglot-ltex-dictionary (concat minemacs-local-dir "eglot/ltex/")))

(defvar eglot-ltex-hidden-false-positives
  (+deserialize-sym
   'eglot-ltex-dictionary (concat minemacs-local-dir "eglot/ltex/")))

(defvar eglot-ltex-disable-rules
  (+deserialize-sym
   'eglot-ltex-dictionary (concat minemacs-local-dir "eglot/ltex/")))

(defun eglot-ltex--can-process-client-commands-a (srv cmd args)
  (let ((rule nil))
    (cond
     ((string= cmd "_ltex.addToDictionary")
      (eglot-ltex--action-add-to-rules args :words 'eglot-ltex-dictionary t)
      (message "[INFO] Word added to dictionary."))
     ((string= cmd "_ltex.hideFalsePositives")
      (eglot-ltex--action-add-to-rules args :falsePositives 'eglot-ltex-hidden-false-positives t)
      (message "[INFO] Rule added to false positives."))
     ((string= cmd "_ltex.disableRules")
      (eglot-ltex--action-add-to-rules args :ruleIds 'eglot-ltex-hidden-disable-rules t)
      (message "[INFO] Rule added to disable rules.")))))

(advice-add 'eglot-execute-command :before-while #'eglot-ltex--can-process-client-commands-a)
;; (advice-remove 'eglot-execute-command #'eglot-ltex-can-process-client-commands-a)

(defvar eglot-ltex-user-rules-path (concat minemacs-local-dir "eglot/ltex/"))

(defun eglot-ltex--add-rule (lang rule rules-plist)
  "Add RULE of language LANG to the plist named RULES-PLIST (symbol)."
  (when (null (eval rules-plist))
    (set rules-plist (list lang [])))
  (plist-put (eval rules-plist) lang
             (vconcat (list rule) (plist-get (eval rules-plist) lang)))
  (when-let (out-file (+serialize-sym rules-plist eglot-ltex-user-rules-path))
    (message "[INFO] Rule for language %s saved to file \"%s\"" (symbol-name lang) out-file)))

(defun eglot-ltex--action-add-to-rules (action key rules-plist &optional store)
  "Execute action ACTION by getting KEY and storing it in the RULES-PLIST.
When STORE is non-nil, this will also store the new plist in the directory
`eglot-ltex-user-rules-path'."
  (let ((args-plist (plist-get (if (vectorp action) (elt action 0) action) key)))
    (dolist (lang (+plist-keys args-plist))
      (mapc (lambda (rule)
              (eglot-ltex--add-rule lang rule rules-plist)
              (when store
                (+serialize-sym rules-plist eglot-ltex-user-rules-path)))
            (plist-get args-plist lang)))))

(provide 'me-eglot-ltex-extras)
