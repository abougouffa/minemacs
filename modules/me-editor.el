;;; me-editor.el --- Editing stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package tempel
  :straight t
  :custom
  (tempel-trigger-prefix "<") ;; Require trigger prefix before template name when completing.
  (tempel-path
   (list (concat minemacs-assets-dir "templates/tempel/*.eld")
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
  (setq highlight-numbers-generic-regexp (rx (and symbol-start (one-or-more digit)) (optional "." (* digit)) symbol-end))

  ;; Define the right format for numbers in `dts-mode'
  (puthash 'dts-mode
           (rx (and symbol-start (or (+ digit) (+ hex-digit) (and "0" (any "xX") (+ hex-digit))) symbol-end))
           highlight-numbers-modelist))

(use-package expreg
  :straight (:host github :repo "casouri/expreg")
  :when (+emacs-features-p 'tree-sitter)
  :init
  (+vmap!
    "v" #'expreg-expand
    "q" #'expreg-contract))

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

(use-package super-save
  :straight t
  :hook (minemacs-first-file . super-save-mode)
  :custom
  (super-save-silent t)
  (super-save-all-buffers t)
  (super-save-auto-save-when-idle t)
  (super-save-delete-trailing-whitespace 'except-current-line)
  :config
  ;; Additional triggers
  (setq super-save-triggers
        (append super-save-triggers
                '(magit magit-status winner-undo winner-redo find-file))))

(use-package selection-highlight-mode
  :straight (:host github :repo "balloneij/selection-highlight-mode")
  :hook (minemacs-first-file . selection-highlight-mode)
  :custom-face (selection-highlight-mode-match-face ((t (:background "lavender")))))

(use-package highlight-indent-guides
  :straight t
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-character #x2506)
  (highlight-indent-guides-responsive 'top))

(use-package vimish-fold
  :straight t
  :hook (minemacs-first-file . vimish-fold-global-mode))

(use-package evil-vimish-fold
  :straight t
  :hook (vimish-fold-global-mode . evil-vimish-fold-mode)
  :when (and (memq 'me-evil minemacs-core-modules) (not (+package-disabled-p 'evil)))
  :commands evil-vimish-fold/next-fold evil-vimish-fold/previous-fold vimish-fold/delete evil-vimish-fold/delete-all evil-vimish-fold/create evil-vimish-fold/create-line
  :custom
  (vimish-fold-dir (concat minemacs-cache-dir "vimish-fold/"))
  (vimish-fold-indication-mode 'right-fringe)
  :init
  (with-eval-after-load 'evil
    (evil-define-key* 'motion 'global
      "zf" #'evil-vimish-fold/create
      "zF" #'evil-vimish-fold/create-line
      "zd" #'vimish-fold-delete
      "zE" #'vimish-fold-delete-all)))

;; Bind `+yank-region-as-paragraph' (autoloaded from "me-lib.el")
(+nvmap! "gy" #'+kill-region-as-paragraph)


(provide 'me-editor)

;;; me-editor.el ends here
