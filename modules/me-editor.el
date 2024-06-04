;;; me-editor.el --- Editing stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package tempel
  :straight t
  :custom
  (tempel-trigger-prefix "<") ;; Require trigger prefix before template name when completing.
  (tempel-path (list (concat minemacs-assets-dir "templates/tempel/*.eld")
                     (concat minemacs-config-dir "templates/tempel/*.eld")))
  :bind (("M-\"" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert)
         :map tempel-map
         ("TAB" . tempel-next)
         ("<backtab>" . tempel-previous))
  :hook ((prog-mode text-mode) . +tempel-setup-capf-h)
  :hook (prog-mode . tempel-abbrev-mode)
  :config
  (defun +tempel-setup-capf-h ()
    (add-hook 'completion-at-point-functions #'tempel-complete -100 t)))

(use-package tempel-collection
  :straight t)

(use-package wgrep
  :straight t
  :commands wgrep-change-to-wgrep-mode
  :custom
  (wgrep-auto-save-buffer t))

(use-package spdx
  :straight (:host github :repo "condy0919/spdx.el")
  :custom
  (spdx-copyright-holder 'user)
  (spdx-project-detection 'auto)
  :init
  (+map!
    "il" #'spdx-insert-spdx-only
    "ic" #'spdx-insert-spdx-copyright))

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-numbers
  :straight t
  :hook ((prog-mode conf-mode) . highlight-numbers-mode)
  :config
  (setq highlight-numbers-generic-regexp (rx (and symbol-start (one-or-more digit)) (optional "." (* digit)) symbol-end))
  ;; Define the right format for numbers in `dts-mode'
  (puthash 'dts-mode
           (rx (and symbol-start (or (+ digit) (+ hex-digit) (and "0" (any "xX") (+ hex-digit))) symbol-end))
           highlight-numbers-modelist))

(use-package zones
  :straight t)

(use-package smartparens
  :straight t
  :hook (minemacs-lazy . smartparens-global-mode)
  :config
  (sp-local-pair 'org-mode "$" "$" :unless '(sp-point-after-word-p))
  (require 'smartparens-config))

(use-package expreg
  :straight (:host github :repo "casouri/expreg")
  :when (+emacs-features-p 'tree-sitter))

;; Fallback to `expand-region' if `expreg' cannot be used
(unless (+emacs-features-p 'tree-sitter)
  (+load minemacs-obsolete-modules-dir "me-expand-region.el"))

(use-package drag-stuff
  :straight t
  :init
  :bind (("M-<up>" . drag-stuff-up)
         ("M-<down>" . drag-stuff-down)
         ("M-<left>" . drag-stuff-left)
         ("M-<right>" . drag-stuff-right)))

(use-package real-backup
  :straight (:host github :repo "abougouffa/real-backup")
  :hook (minemacs-first-file . real-backup-mode))

(use-package cc-isearch-menu
  :straight t
  :bind (:package isearch :map isearch-mode-map ([f2] . cc-isearch-menu-transient)))


(provide 'me-editor)

;;; me-editor.el ends here
