;;; me-eglot-ltex.el --- Extra functionality for Eglot+LTeX-LS -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;; This file provides a hackish implementation of some of `ltex-ls-plus'
;; commands which needs to be handled by the client. See:
;; https://ltex-plus.github.io/ltex-plus/ltex-ls-plus/server-usage.html#commands

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

(defgroup minemacs-eglot-ltex nil
  "LTeX+ LS related settings."
  :group 'minemacs-utils)

(defcustom eglot-ltex-user-rules-path (concat minemacs-local-dir "eglot/ltex/")
  "Path to save user rules."
  :group 'minemacs-eglot-ltex
  :type 'directory)

(defcustom eglot-ltex-ngrams-path nil
  "Path the language model's ngrams."
  :group 'minemacs-eglot-ltex
  :type 'directory)

(defcustom eglot-ltex-ls-path (expand-file-name "ltex-ls-plus/" minemacs-local-dir)
  "Path to LTeX+ LS."
  :group 'minemacs-eglot-ltex
  :type 'directory)

(defcustom eglot-ltex-ls-program (expand-file-name "bin/ltex-ls-plus" eglot-ltex-ls-path)
  "The path or executable name of the LTeX+ LS."
  :group 'minemacs-eglot-ltex
  :type '(choice file string))

;;;###autoload
(defun eglot-ltex-ls-install (pre)
  "Download the latest release of LTeX+ LS.

When PRE is non-nil, allow downloading the latest prerelease."
  (interactive "P")
  (when-let* ((tarball (+github-download-release
                        "ltex-plus/ltex-ls-plus"
                        (concat "-" (cond ((+emacs-options-p 'os/linux) "linux")
                                          ((+emacs-options-p 'os/mac) "mac")
                                          ((+emacs-options-p 'os/win) "windows"))
                                "-" (cond ((+emacs-options-p 'arch/x86_64) "x64")
                                          ((+emacs-options-p 'arch/x86_64) "aarch64")))
                        nil :prerelease pre)))
    (when (file-directory-p eglot-ltex-ls-path) (delete-directory eglot-ltex-ls-path t))
    (mkdir eglot-ltex-ls-path)
    (let ((compilation-buffer-name-function (lambda (_a) "*ltex-ls-plus:install*")))
      (compile (format "tar -C %S -xf %S --strip-components=2" eglot-ltex-ls-path tarball)))))

(defvar-local eglot-ltex-language "auto")
;;;###autoload(put 'eglot-ltex-language 'safe-local-variable 'stringp)

;; Load serialized rules
(defvar eglot-ltex-dictionary
  (+deserialize-sym 'eglot-ltex-dictionary eglot-ltex-user-rules-path))

(defvar eglot-ltex-hidden-false-positives
  (+deserialize-sym 'eglot-ltex-hidden-false-positives eglot-ltex-user-rules-path))

(defvar eglot-ltex-disable-rules
  (+deserialize-sym 'eglot-ltex-disable-rules eglot-ltex-user-rules-path))

(defun eglot-ltex--process-client-commands:before-a (_server action)
  "Advice for `eglot-execute' to process LTeX-LS client ACTION."
  (let* ((action (if (plist-get action :kind) ; unpack if necessary
                     (plist-get action :command)
                   action))
         (cmd (plist-get action :command))
         (args (plist-get action :arguments)))
    (cond
     ((string= cmd "_ltex.addToDictionary")
      (eglot-ltex--action-add-to-rules args :words 'eglot-ltex-dictionary 'store)
      (message "Word added to dictionary."))
     ((string= cmd "_ltex.hideFalsePositives")
      (eglot-ltex--action-add-to-rules args :falsePositives 'eglot-ltex-hidden-false-positives 'store)
      (message "Rule added to false positives."))
     ((string= cmd "_ltex.disableRules")
      (eglot-ltex--action-add-to-rules args :ruleIds 'eglot-ltex-disable-rules 'store)
      (message "Rule added to disable rules.")))))

(defun eglot-ltex-workspace-config-fn (&optional _server)
  "A function to use as a value of `eglot-workspace-configuration'.
It generates the workspace configuration dynamically, taking into account
changed values of `eglot-ltex-language', `eglot-ltex-dictrionary', and so on."
  `(:ltex
    ;; Enable all supported lanugage IDs by default, the full list here:
    ;; https://github.com/ltex-plus/ltex-ls-plus/blob/develop/src/main/kotlin/org/bsplines/ltexls/parsing/CodeFragmentizer.kt
    (:enabled
     ["asciidoc"
      "bib" "bibtex"
      "gitcommit" "git-commit"
      "html" "xhtml"
      "context" "context.tex" "latex" "tex" "plaintex" "rsweave"
      "markdown" "mdx" "quatro" "rmd"
      "nop"
      "org"
      "restructuredtext"
      "typst"
      "plaintext"]
     :language ,eglot-ltex-language
     :dictionary ,eglot-ltex-dictionary
     :disabledRules ,eglot-ltex-disable-rules
     :hiddenFalsePositives ,eglot-ltex-hidden-false-positives
     :additionalRules
     (:languageModel
      ,(if (and (stringp eglot-ltex-ngrams-path) (file-directory-p eglot-ltex-ngrams-path))
           eglot-ltex-ngrams-path
         "/usr/share/ngrams/")))))

(defun eglot-ltex--add-rule (lang rule rules-plist)
  "Add RULE of language LANG to the plist named RULES-PLIST (symbol)."
  (when (null (eval rules-plist))
    (set rules-plist (list lang [])))
  (plist-put (eval rules-plist) lang
             (vconcat (list rule) (plist-get (eval rules-plist) lang)))
  (when-let* ((out-file (+serialize-sym rules-plist eglot-ltex-user-rules-path)))
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
  (advice-add 'eglot-execute :before #'eglot-ltex--process-client-commands:before-a))

(defun eglot-ltex-disable-handling-client-commands ()
  "Disable Eglot hack to handle code actions of LTeX-LS."
  (interactive)
  (advice-remove 'eglot-execute #'eglot-ltex--process-client-commands:before-a))


(provide 'me-eglot-ltex)

;;; me-eglot-ltex.el ends here
