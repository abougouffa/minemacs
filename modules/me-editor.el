;;; me-editor.el --- Editing stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(use-package tempel
  :straight t
  :custom
  (tempel-trigger-prefix "<") ;; Require trigger prefix before template name when completing.
  (tempel-path
   (list (concat minemacs-root-dir "assets/templates/tempel/*.eld")
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
  :straight t
  :after tempel
  :demand t)

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-numbers
  :straight t
  :hook ((prog-mode conf-mode) . highlight-numbers-mode)
  :config
  (setq highlight-numbers-generic-regexp (rx (and symbol-start (one-or-more digit)) (optional "." (* digit)) symbol-end)))

(use-package expreg
  :straight (:host github :repo "casouri/expreg")
  :when (+emacs-features-p 'tree-sitter)
  :init
  (+vmap!
    "v" #'expreg-expand
    "q" #'expreg-contract))

;; Fallback to `expand-region' if `expreg' cannot be used
(unless (+emacs-features-p 'tree-sitter)
  (+load minemacs-modules-dir "obsolete/me-expand-region.el"))

(use-package drag-stuff
  :straight t
  :init
  :bind (("M-<up>" . drag-stuff-up)
         ("M-<down>" . drag-stuff-down)
         ("M-<left>" . drag-stuff-left)
         ("M-<right>" . drag-stuff-right)))

(use-package super-save
  :straight t
  :hook (minemacs-first-file . super-save-mode)
  :custom
  (super-save-silent t)
  (super-save-all-buffers t)
  (super-save-idle-duration 15)
  (super-save-auto-save-when-idle t)
  (super-save-delete-trailing-whitespaces 'except-current-line))

;; Bind `+yank-region-as-paragraph' (autoloaded from "me-lib.el")
(+nvmap! "gy" #'+kill-region-as-paragraph)


(provide 'me-editor)

;;; me-editor.el ends here
