;;; me-eglot-ltex.el --- Extra functionality for Eglot+LTeX-LS -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;; This file provides a hackish implementation of some of `ltex-ls' commands
;; which needs to be handled by the client.
;; See: valentjn.github.io/ltex/ltex-ls/server-usage.html#commands

;; This file implements these code actions:
;; * _ltex.addToDictionary
;; * _ltex.disableRules
;; * _ltex.hideFalsePositives

;; The custom actions are executed via an advice to `eglot-execute-command'. I
;; didn't find a direct way to register custom handler.

;; In theory, the disabled rules and custom dictionary can be saved to a word
;; list and then, they can be passed to ltex-ls as ":/path/to/dictionary.txt".
;; However, in practice, I didn't manage to get it to work with external files.
;; The workaround I use is to store them in a plist, serialize it, and load it
;; at startup. Files are stored under `eglot-ltex-user-rules-path'.

;; To use this with French for example, add something like this to the relevant
;; .dir-locals.el file:
;; ((org-mode . ((eglot-workspace-configuration . eglot-ltex-workspace-config-fn)
;;               (eglot-ltex-language . "fr"))))

;;; Code:

(defvar-local eglot-ltex-language "auto")
(defvar eglot-ltex-user-rules-path (concat minemacs-local-dir "eglot/ltex/"))

(defvar ltex-ls-command "ltex-ls")
(defvar ltex-ls-server-port 40001)
(defvar ltex-ls-server-process-name "ltex-ls-server")
(defvar ltex-ls--process nil)

;;;###autoload
(defun ltex-ls-start ()
  "Start LTeX-LS as a TCP server at port `ltex-ls-server-port' on \"localhost\"."
  (interactive)
  (if (eq (process-status ltex-ls-server-process-name) 'run)
      (message "LTeX-LS server already running!")
    (if (executable-find ltex-ls-command)
        (setq ltex-ls--process
              (make-process
               :name ltex-ls-server-process-name
               :buffer (format " *%s*" ltex-ls-server-process-name)
               :command (list ltex-ls-command "--server-type=TcpSocket" (format "--port=%d" ltex-ls-server-port))))
      (user-error "LTeX-LS command \"%s\" not found." ltex-ls-command))
    (if (process-live-p ltex-ls--process)
        (message "Started LTeX-LS TCP server successfuly.")
      (user-error "Cannot start LTeX-LS TCP server."))))

(defun ltex-ls-stop ()
  "Stop LTeX-LS server if running."
  (interactive)
  (if (not (process-live-p ltex-ls--process))
      (message "No running instance of LTeX-LS server!")
    (delete-process ltex-ls--process)
    (message "LTeX-LS server stopped.")))

(defvar eglot-ltex-dictionary
  (+deserialize-sym 'eglot-ltex-dictionary eglot-ltex-user-rules-path))

(defvar eglot-ltex-hidden-false-positives
  (+deserialize-sym 'eglot-ltex-hidden-false-positives eglot-ltex-user-rules-path))

(defvar eglot-ltex-disable-rules
  (+deserialize-sym 'eglot-ltex-disable-rules eglot-ltex-user-rules-path))

(defun eglot-ltex--can-process-client-commands-a (srv cmd args)
  (cond
   ((string= cmd "_ltex.addToDictionary")
    (eglot-ltex--action-add-to-rules args :words 'eglot-ltex-dictionary t)
    (message "[INFO] Word added to dictionary."))
   ((string= cmd "_ltex.hideFalsePositives")
    (eglot-ltex--action-add-to-rules args :falsePositives 'eglot-ltex-hidden-false-positives t)
    (message "[INFO] Rule added to false positives."))
   ((string= cmd "_ltex.disableRules")
    (eglot-ltex--action-add-to-rules args :ruleIds 'eglot-ltex-disable-rules t)
    (message "[INFO] Rule added to disable rules."))))

(defun eglot-ltex-workspace-config-fn (&optional _server)
  `(:ltex
    (:language ,eglot-ltex-language
     :dictionary ,eglot-ltex-dictionary
     :disabledRules ,eglot-ltex-disable-rules
     :hiddenFalsePositives ,eglot-ltex-hidden-false-positives
     :additionalRules (:languageModel "/usr/share/ngrams/"))))

(defun eglot-ltex--add-rule (lang rule rules-plist)
  "Add RULE of language LANG to the plist named RULES-PLIST (symbol)."
  (when (null (eval rules-plist))
    (set rules-plist (list lang [])))
  (plist-put (eval rules-plist) lang
             (vconcat (list rule) (plist-get (eval rules-plist) lang)))
  (when-let (out-file (+serialize-sym rules-plist eglot-ltex-user-rules-path))
    (+log! "[eglot-ltex] Rule for language %s saved to file \"%s\"" (symbol-name lang) out-file)))

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

(defun eglot-ltex-enable-handling-client-commands ()
  "Enable Eglot hack to handle code actions of LTeX-LS."
  (interactive)
  (advice-add 'eglot-execute-command :before #'eglot-ltex--can-process-client-commands-a))

(defun eglot-ltex-disable-handling-client-commands ()
  "Disable Eglot hack to handle code actions of LTeX-LS."
  (interactive)
  (advice-remove 'eglot-execute-command #'eglot-ltex--can-process-client-commands-a))


(provide 'me-eglot-ltex)

;;; me-eglot-ltex.el ends here
