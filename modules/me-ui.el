;;; me-ui.el --- UI stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa and contributors

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

(use-package nerd-icons-ibuffer
  :straight t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package pulsar
  :straight t
  :hook (minemacs-after-startup . pulsar-global-mode)
  :config
  (with-eval-after-load 'evil
    (setq pulsar-pulse-functions
          (append pulsar-pulse-functions
                  '(evil-yank evil-paste-after evil-paste-before
                    evil-delete evil-delete-line evil-delete-whole-line
                    evil-goto-last-change evil-goto-last-change-reverse)))))

;; From Doom Emacs
(use-package anzu
  :straight t
  :custom
  (anzu-cons-mode-line-p nil) ; We manage our own modeline segments
  :config
  ;; Ensure anzu state is cleared when searches & iedit are done
  (add-hook 'iedit-mode-end-hook #'anzu--reset-status)
  (advice-add #'evil-force-normal-state :before #'anzu--reset-status)
  ;; Fix matches segment mirroring across all buffers
  (mapc #'make-variable-buffer-local
        '(anzu--total-matched anzu--current-position anzu--state anzu--cached-count
          anzu--cached-positions anzu--last-command anzu--last-isearch-string anzu--overflow-p)))

(use-package evil-anzu
  :straight t
  :hook (evil-mode . global-anzu-mode))


(provide 'me-ui)

;;; me-ui.el ends here
