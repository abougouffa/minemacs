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
  :bind (("M-S-<up>" . drag-stuff-up)
         ("M-S-<down>" . drag-stuff-down)
         ("M-S-<left>" . drag-stuff-left)
         ("M-S-<right>" . drag-stuff-right)))

(use-package auto-save
  :straight (:host github :repo "manateelazycat/auto-save")
  :hook (minemacs-first-file . auto-save-enable)
  :custom
  (auto-save-idle 3)
  (auto-save-silent t)
  (auto-save-delete-trailing-whitespace t))

;; Bind `+yank-region-as-paragraph' (autoloaded from "me-lib.el")
(+nvmap! "gy" #'+kill-region-as-paragraph)


(provide 'me-editor)

;;; me-editor.el ends here
