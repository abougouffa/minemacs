;;; me-ui.el --- UI stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package lacarte
  :straight t
  :bind ([f10] . lacarte-execute-menu-command))

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
  (setq
   mixed-pitch-fixed-pitch-faces
   (delete-dups
    (append
     mixed-pitch-fixed-pitch-faces
     '(font-lock-comment-delimiter-face font-lock-comment-face org-block
       org-block-begin-line org-block-end-line org-cite org-cite-key
       org-document-info-keyword org-done org-drawer org-footnote org-formula
       org-inline-src-block org-latex-and-related org-link org-code org-column
       org-column-title org-date org-macro org-meta-line org-property-value
       org-quote org-ref-cite-face org-sexp-date org-special-keyword org-src
       org-table org-tag org-tag-group org-todo org-verbatim org-verse)))))

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

(use-package logos
  :straight t
  :custom
  ;; If you want to use outlines instead of page breaks (the ^L):
  (logos-outlines-are-pages t)
  ;; This is the default value for the outlines:
  (logos-outline-regexp-alist `((emacs-lisp-mode . "^;;;+ ")
                                (org-mode . "^\\*+ +")
                                (markdown-mode . "^\\#+ +")))
  ;; These apply when `logos-focus-mode' is enabled.  Their value is buffer-local.
  (logos-hide-cursor nil)
  (logos-hide-mode-line t)
  (logos-hide-header-line t)
  (logos-hide-buffer-boundaries t)
  (logos-hide-fringe t)
  (logos-variable-pitch nil)
  (logos-buffer-read-only nil)
  (logos-scroll-lock nil)
  :init
  (let ((map global-map))
    (define-key map [remap narrow-to-region] #'logos-narrow-dwim)
    (define-key map [remap forward-page] #'logos-forward-page-dwim)
    (define-key map [remap backward-page] #'logos-backward-page-dwim)
    (define-key map (kbd "<f9>") #'logos-focus-mode)))

(use-package nerd-icons-ibuffer
  :straight t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package pulsar
  :straight t
  :hook (minemacs-first-file . pulsar-global-mode)
  :config
  (with-eval-after-load 'evil
    (setq pulsar-pulse-functions
          (append pulsar-pulse-functions
                  '(evil-yank evil-paste-after evil-paste-before
                    evil-delete evil-delete-line evil-delete-whole-line
                    evil-goto-last-change evil-goto-last-change-reverse)))))


(provide 'me-ui)

;;; me-ui.el ends here
