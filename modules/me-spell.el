;; -*- lexical-binding: t; -*-

(defconst ASPELL-P (executable-find "aspell"))

(use-package spell-fu
  :straight t
  :when ASPELL-P
  :general
  (me-map "ts" #'spell-fu-mode)
  :hook (text-mode . spell-fu-mode)
  :init
  (setq spell-fu-directory (expand-file-name "spell-fu" minemacs-local-dir))
  (defvar +spell-excluded-faces-alist
    '((markdown-mode
       . (markdown-code-face
          markdown-html-attr-name-face
          markdown-html-attr-value-face
          markdown-html-tag-name-face
          markdown-inline-code-face
          markdown-link-face
          markdown-markup-face
          markdown-plain-url-face
          markdown-reference-face
          markdown-url-face))
      (org-mode
       . (org-block
          org-block-begin-line
          org-block-end-line
          org-cite
          org-cite-key
          org-code
          org-date
          org-footnote
          org-formula
          org-inline-src-block
          org-latex-and-related
          org-link
          org-meta-line
          org-property-value
          org-ref-cite-face
          org-special-keyword
          org-tag
          org-todo
          org-todo-keyword-done
          org-todo-keyword-habt
          org-todo-keyword-kill
          org-todo-keyword-outd
          org-todo-keyword-todo
          org-todo-keyword-wait
          org-verbatim))
      (latex-mode
       . (font-latex-math-face
          font-latex-sedate-face
          font-lock-function-name-face
          font-lock-keyword-face
          font-lock-variable-name-face)))
    "Faces in certain major modes that spell-fu will not spellcheck.")
  :config
  (unless (file-exists-p spell-fu-directory)
    (mkdir spell-fu-directory t))

  (defun me-spell-fu-register-dictionary (lang)
    "Add `LANG` to spell-fu multi-dict, with a personal dictionary."
    ;; Add the dictionary
    (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary lang))
    (let ((personal-dict-file (expand-file-name (format "personal-aspell.%s.pws" lang) spell-fu-directory)))
      ;; Create an empty personal dictionary if it doesn't exists
      (unless (file-exists-p personal-dict-file) (write-region "" nil personal-dict-file))
      ;; Add the personal dictionary
      (spell-fu-dictionary-add (spell-fu-get-personal-dictionary (format "%s-personal" lang) personal-dict-file))))

  (add-hook
   'spell-fu-mode-hook
   (defun +spell-fu--init-excluded-faces-h ()
     "Set `spell-fu-faces-exclude' according to `+spell-excluded-faces-alist'."
     (when-let (excluded (cdr (cl-find-if #'derived-mode-p +spell-excluded-faces-alist :key #'car)))
       (setq-local spell-fu-faces-exclude excluded)))))


(use-package me-spell-fu
  :after spell-fu
  :general
  (me-map-key "z=" #'+spell-fu-correct))


;; Add this to .dir-locals.el
;; ((nil (eglot-workspace-configuration
;;        . ((ltex . ((language . "fr")
;;                    (disabledRules . ((fr . ["FRENCH_WHITESPACE"])))
;;                    (additionalRules . ((languageModel . "/usr/share/ngrams/")))))))))
(use-package eglot-ltex
  :straight (:host github :repo "emacs-languagetool/eglot-ltex")
  :after eglot
  :custom
  (eglot-languagetool-server-path "/usr")
  :init
  (add-hook
   'org-mode-hook
   (defun +eglot-ltex--set-workspace ()
     (setq-local
      eglot-workspace-configuration
      '(:ltex (:language "fr"))))))


(provide 'me-spell)
