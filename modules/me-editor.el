;;; me-editor.el --- Editing stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package ws-butler
  :straight (:host github :repo "hlissner/ws-butler")
  :hook (minemacs-first-file . ws-butler-global-mode))

(use-package dtrt-indent
  :straight t
  :hook (minemacs-first-file . dtrt-indent-global-mode)
  :custom
  (dtrt-indent-verbosity (if minemacs-verbose-p 3 0)))

(use-package yasnippet
  :straight t
  :hook ((text-mode prog-mode conf-mode) . yas-minor-mode)
  :init
  (defvar yas-verbosity (if minemacs-verbose-p 4 2))
  (unless minemacs-verbose-p
    (+fn-inhibit-messages! yas-define-snippets) ; suppress "Multiple snippets with same identity: ..."
    (+fn-inhibit-messages! yas--parse-template)) ; suppress "Ignoring unknown directive ..."
  :custom
  (yas-triggers-in-field t) ; Allow nested snippets
  (yas-snippet-dirs (list (+directory-ensure minemacs-config-dir "snippets/") (concat minemacs-root-dir "snippets/"))))

(use-package yasnippet-capf
  :straight t
  :hook ((prog-mode text-mode conf-mode) . +cape-yasnippet--setup-h)
  :bind (("C-c p y" . yasnippet-capf))
  :init
  (defun +cape-yasnippet--setup-h ()
    (when (bound-and-true-p yas-minor-mode)
      (add-to-list 'completion-at-point-functions #'yasnippet-capf))))

(use-package yasnippet-snippets
  :straight t)

(use-package doom-snippets
  :straight (:host github :repo "hlissner/doom-snippets" :files ("*.el" "*")))

(use-package license-snippets
  :straight t
  :after yasnippet
  :init
  (license-snippets-init))

(use-package spdx
  :straight (:host github :repo "condy0919/spdx.el")
  :custom
  (spdx-copyright-holder 'user)
  (spdx-project-detection 'auto))

(use-package wgrep
  :straight t
  :commands wgrep-change-to-wgrep-mode
  :custom
  (wgrep-auto-save-buffer t))

(use-package symbol-overlay
  :straight t)

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

(use-package selection-highlight-mode
  :straight (:host github :repo "balloneij/selection-highlight-mode")
  :hook (minemacs-lazy . selection-highlight-mode)
  :init
  (add-hook
   'enable-theme-functions
   (satch-defun +selection-highlight--set-face-h (&rest _)
     (with-eval-after-load 'selection-highlight-mode
       (let* ((hsl (apply #'color-rgb-to-hsl (color-name-to-rgb (face-attribute 'region :background nil t))))
              (luminance (nth 2 hsl))
              (luminance
               (if (eq 'light (frame-parameter nil 'background-mode))
                   ;; On light themes, make it brighter
                   (min 1.0 (* 1.09 luminance))
                 ;; On dark themes, make it darker
                 (* 0.91 luminance)))
              (new-rgb (color-hsl-to-rgb (nth 0 hsl) (nth 1 hsl) luminance)))
         (set-face-background
          'selection-highlight-mode-match-face
          (apply #'color-rgb-to-hex (append new-rgb '(2)))))))))

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
  :when (+emacs-features-p 'tree-sitter)
  :bind (("C-M-SPC" . expreg-expand) ; orig. `mark-sexp'
         ("S-C-M-SPC" . expreg-contract)))

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

(use-package editorconfig
  :straight t
  :hook (minemacs-first-file . editorconfig-mode)
  :config
  ;; Exclude compressed files
  (push "\\.\\(zip\\|epub\\|\\(doc\\|xls\\|ppt\\)x\\)\\'" editorconfig-exclude-regexps))


(provide 'me-editor)

;;; me-editor.el ends here
