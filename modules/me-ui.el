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
                 '(font-lock-comment-delimiter-face
                   font-lock-comment-face
                   org-block
                   org-block-begin-line
                   org-block-end-line
                   org-cite
                   org-cite-key
                   org-code
                   org-column
                   org-column-title
                   org-date
                   org-document-info-keyword
                   org-done
                   org-drawer
                   org-footnote
                   org-formula
                   org-inline-src-block
                   org-latex-and-related
                   org-link
                   org-macro
                   org-meta-line
                   org-property-value
                   org-quote
                   org-ref-cite-face
                   org-sexp-date
                   org-special-keyword
                   org-src
                   org-table
                   org-tag
                   org-tag-group
                   org-todo
                   org-verbatim
                   org-verse)))))

(use-package me-writing-mode
  :init
  (+map!
    "tw" #'+writing-mode
    "tW" #'+writing-global-mode))

(use-package page-break-lines
  :straight t
  :hook ((prog-mode text-mode special-mode) . page-break-lines-mode))

(use-package focus
  :straight t
  :init
  (+map! "tF" #'focus-mode))

(use-package nerd-icons-ibuffer
  :straight t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))


(provide 'me-ui)

;;; me-ui.el ends here
