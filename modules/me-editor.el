;;; me-editor.el --- Editing stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;; Visualize and navigate the undo tree
(use-package vundo
  :straight t
  :bind (:map minemacs-open-thing-map ("u" . vundo))
  :custom
  (vundo-compact-display t)
  (vundo-window-max-height 8)
  (vundo-glyph-alist vundo-unicode-symbols))


;; Persistent undo tree between sessions
(use-package undo-fu-session
  :straight t
  :hook (minemacs-lazy . undo-fu-session-global-mode)
  :custom
  (undo-fu-session-compression (if (executable-find "zstd") 'zst 'gz)))


;; Unobtrusively trim extraneous white-space *ONLY* in lines edited
(use-package ws-butler
  :straight (:host github :repo "hlissner/ws-butler")
  :hook (minemacs-first-file . ws-butler-global-mode))


;; Smart guessing the indentation offset originally used in the opened source files
(use-package dtrt-indent
  :straight t
  :after minemacs-first-file
  :hook ((change-major-mode-after-body read-only-mode) . +dtrt-indent-mode-maybe)
  :custom
  (dtrt-indent-max-lines 2500) ; Faster than the default 5000
  (dtrt-indent-run-after-smie t)
  :init
  ;; Better predicates for enabling `dtrt-indent', inspired by Doom Emacs
  (defvar +dtrt-indent-excluded-modes
    '(emacs-lisp-mode
      lisp-mode
      pascal-mode
      ;; Automatic indent detection in Org files is meaningless. Not to mention,
      ;; a non-standard `tab-width' causes an error in `org-mode.'
      org-mode
      ;; Indent detection is slow and inconclusive in coq-mode files, and rarely
      ;; helpful anyway, so inhibit it (see doomemacs/doomemacs#5823).
      coq-mode)
    "A list of major modes where indentation shouldn't be auto-detected.")
  (defun +dtrt-indent-mode-maybe ()
    (unless (or (not (featurep 'minemacs-first-file))
                (memq major-mode '(fundamental-mode guard-lf-large-file-mode so-long-mode))
                (member (substring (buffer-name) 0 1) '(" " "*"))
                (apply #'derived-mode-p +dtrt-indent-excluded-modes))
      ;; Don't display messages in the echo area, but still log them
      (let ((inhibit-message (not minemacs-debug-p)))
        (dtrt-indent-mode +1)))))


;; A template system for Emacs
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


;; Completion-At-Point Extension for YASnippet
(use-package yasnippet-capf
  :straight t
  :hook ((prog-mode text-mode conf-mode) . +cape-yasnippet--setup-h)
  :bind (("C-c p y" . yasnippet-capf))
  :init
  (defun +cape-yasnippet--setup-h ()
    (when (bound-and-true-p yas-minor-mode)
      (add-to-list 'completion-at-point-functions #'yasnippet-capf))))


;; A collection of yasnippet snippets for many languages
(use-package yasnippet-snippets
  :straight t)


;; The Doom Emacs snippets library
(use-package doom-snippets
  :straight (:host github :repo "hlissner/doom-snippets" :files ("*.el" "*")))


;; Insert SPDX license header
(use-package spdx
  :straight (:host github :repo "condy0919/spdx.el")
  :custom
  (spdx-copyright-holder 'user)
  (spdx-project-detection 'auto))


;; Writable grep buffer and apply the changes to files
(use-package wgrep
  :straight t
  :commands (wgrep-change-to-wgrep-mode)
  :custom
  (wgrep-auto-save-buffer t))


;; Highlight symbols with keymap-enabled overlays
(use-package symbol-overlay
  :straight t)


;; Emacs rainbow delimiters mode
(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))


;; Highlight numbers in source code
(use-package highlight-numbers
  :straight t
  :hook ((prog-mode conf-mode) . highlight-numbers-mode)
  :config
  (setq highlight-numbers-generic-regexp (rx (and symbol-start (one-or-more digit)) (optional "." (* digit)) symbol-end))
  ;; Define the right format for numbers in `dts-mode'
  (puthash 'dts-mode
           (rx (and symbol-start (or (+ digit) (+ hex-digit) (and "0" (any "xX") (+ hex-digit))) symbol-end))
           highlight-numbers-modelist))


;; An Emacs minor mode for highlighting matches to the selection
(use-package selection-highlight-mode
  :straight (:host github :repo "balloneij/selection-highlight-mode" :fork (:repo "abougouffa/selection-highlight-mode"))
  :hook (minemacs-lazy . selection-highlight-mode)
  :init
  ;; Automatically set the face for `selection-highlight-mode'
  (satch-add-hook
   '(enable-theme-functions disable-theme-functions server-after-make-frame-hook)
   (satch-defun +selection-highlight--set-face-h (&rest _args)
     (with-eval-after-load 'selection-highlight-mode
       (with-eval-after-load 'isearch
         (when (display-graphic-p)
           (require 'color)
           (let ((new-color (funcall
                             (if (eq 'light (frame-parameter nil 'background-mode))
                                 #'color-lighten-name
                               #'color-darken-name)
                             (face-attribute 'isearch :background nil t)
                             5)))
             (set-face-background 'selection-highlight-mode-match-face new-color)
             (set-face-background 'selection-highlight-mode-alternate-match-face new-color))))))))


;; Zones of text - like multiple regions
(use-package zones
  :straight t)


;; Minor mode for Emacs that deals with parens pairs and tries to be smart about it
(use-package smartparens
  :straight t
  :hook (minemacs-lazy . smartparens-global-mode)
  :custom
  (sp-ignore-modes-list '(minibuffer-inactive-mode)) ; Enable in `minibuffer-mode'
  :config
  (sp-local-pair 'org-mode "$" "$" :unless '(sp-point-after-word-p))
  (require 'smartparens-config))


;; Your friendly neighborhood expand-region clone
(use-package expreg
  :straight (:host github :repo "casouri/expreg")
  :when (+emacs-features-p 'tree-sitter)
  :bind (("C-M-SPC" . expreg-expand) ; orig. `mark-sexp'
         ("S-C-M-SPC" . expreg-contract)))

;; Fallback to `expand-region' if `expreg' cannot be used
(unless (+emacs-features-p 'tree-sitter)
  (+load minemacs-obsolete-modules-dir "me-expand-region.el"))


;; Drag stuff around in Emacs
(use-package drag-stuff
  :straight t
  :init
  :bind (("M-<up>" . drag-stuff-up)
         ("M-<down>" . drag-stuff-down)
         ("M-<left>" . drag-stuff-left)
         ("M-<right>" . drag-stuff-right)))


;; Perform a backup on each file save, real backup for Emacs!
(use-package real-backup
  :straight (:host github :repo "abougouffa/real-backup")
  :hook (minemacs-first-file . real-backup-mode))


(provide 'me-editor)

;;; me-editor.el ends here
