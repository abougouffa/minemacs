;;; me-ui.el --- UI stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(use-package svg-lib
  :straight t
  :custom
  (svg-lib-icons-dir (concat minemacs-cache-dir "svg-lib/icons/")))

(use-package visual-fill-column
  :straight t
  :custom
  (visual-fill-column-width nil)
  (visual-fill-column-center-text t))

(use-package mixed-pitch
  :straight t
  :init
  (+map! "tm" #'mixed-pitch-mode)
  :custom
  (mixed-pitch-variable-pitch-cursor 'box)
  :config
  (setq mixed-pitch-fixed-pitch-faces
        (delete-dups
         (append mixed-pitch-fixed-pitch-faces
                 '(org-date
                   org-footnote
                   org-drawer
                   org-special-keyword
                   org-property-value
                   org-column-title
                   org-column
                   org-cite
                   org-cite-key
                   org-ref-cite-face
                   org-tag
                   org-table
                   org-tag-group
                   org-formula
                   org-meta-line
                   org-document-info-keyword
                   org-block
                   org-block-begin-line
                   org-block-end-line
                   org-inline-src-block
                   org-src
                   org-verbatim
                   org-code
                   org-quote
                   org-verse
                   org-latex-and-related
                   org-macro
                   org-link
                   org-sexp-date
                   org-todo
                   org-done
                   font-lock-comment-face
                   font-lock-comment-delimiter-face)))))

(use-package me-writing-mode
  :init
  (+map! "tw" #'+writing-mode))

(use-package page-break-lines
  :straight t
  :hook ((prog-mode text-mode special-mode) . page-break-lines-mode))

(use-package focus
  :straight t
  :init
  (+map! "tF" #'focus-mode))


(provide 'me-ui)

;;; me-ui.el ends here
