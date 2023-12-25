;;; me-spell-fu.el --- Spell-Fu -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(use-package spell-fu
  :straight t
  :when (executable-find "aspell")
  :hook (text-mode . spell-fu-mode)
  :hook (spell-fu-mode . +spell-fu--init-excluded-faces-h)
  :custom
  (spell-fu-directory (+directory-ensure minemacs-local-dir "spell-fu/"))
  (spell-fu-word-delimit-camel-case t)
  :init
  (+map! "ts" #'spell-fu-mode)
  (+nvmap! "z=" #'+spell-fu-correct) ; autoloaded from `me-spell-fu'

  (defcustom +spell-excluded-faces-alist
    '((markdown-mode
       . (markdown-code-face markdown-html-attr-name-face markdown-html-attr-value-face
          markdown-html-tag-name-face markdown-inline-code-face markdown-link-face
          markdown-markup-face markdown-plain-url-face markdown-reference-face markdown-url-face))
      (org-mode
       . (org-block org-block-begin-line org-block-end-line org-cite org-cite-key org-code
          org-date org-footnote org-formula org-inline-src-block org-latex-and-related org-link
          org-meta-line org-property-value org-ref-cite-face org-special-keyword org-tag org-todo
          org-todo-keyword-done org-todo-keyword-habt org-todo-keyword-kill org-todo-keyword-outd
          org-todo-keyword-todo org-todo-keyword-wait org-verbatim))
      (latex-mode
       . (font-latex-math-face font-latex-sedate-face font-lock-function-name-face
          font-lock-keyword-face font-lock-variable-name-face)))
    "Faces in certain major modes that spell-fu will not spellcheck."
    :group 'minemacs-ui
    :type '(repeat (cons symbol (repeat face))))
  :config
  (defun +spell-fu--init-excluded-faces-h ()
    "Set `spell-fu-faces-exclude' according to `+spell-excluded-faces-alist'."
    (when-let (excluded (cdr (cl-find-if #'derived-mode-p +spell-excluded-faces-alist :key #'car)))
      (setq-local spell-fu-faces-exclude excluded))))


(provide 'obsolete/me-spell-fu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; me-cov.el ends here
