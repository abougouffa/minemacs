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
  :commands (+dtrt-indent-tab-to-tab-stop)
  :bind (([remap tab-to-tab-stop] . +dtrt-indent-tab-to-tab-stop)) ; Smarter M-i
  :custom
  (dtrt-indent-max-lines 2500) ; Faster than the default 5000
  (dtrt-indent-run-after-smie t)
  :init
  (defvar +dtrt-indent-tab-context-lines 20 "The number of lines above and below point to consider in `+dtrt-indent-tab-to-tab-stop'.")
  ;; Better predicates for enabling `dtrt-indent', inspired by Doom Emacs
  (defvar +dtrt-indent-excluded-modes
    '(emacs-lisp-mode
      lisp-mode
      pascal-mode
      org-mode ; a non-standard `tab-width' causes an error in `org-mode'.
      coq-mode) ; see doomemacs/doomemacs#5823
    "A list of major modes where indentation shouldn't be auto-detected.")
  (defun +dtrt-indent-mode-maybe ()
    (unless (or (not (featurep 'minemacs-first-file))
                (memq major-mode '(fundamental-mode guard-lf-large-file-mode so-long-mode))
                (member (substring (buffer-name) 0 1) '(" " "*"))
                (apply #'derived-mode-p +dtrt-indent-excluded-modes))
      ;; Don't display messages in the echo area, but still log them
      (let ((inhibit-message (not minemacs-verbose-p)))
        (dtrt-indent-mode +1))))
  :config
  (defun +dtrt-indent-tab-to-tab-stop ()
    "Like `tab-to-tab-stop', but set `indent-tabs-mode' according the context.
In some files, there is a mix of spaces and tabs. This uses
`dtrt-indent' to detect which one to insert at point."
    (interactive)
    (let* ((lang (cadr (dtrt-indent--search-hook-mapping major-mode)))
           (result
            (and lang
                 (save-excursion
                   (save-restriction
                     (narrow-to-region
                      (line-beginning-position (- +dtrt-indent-tab-context-lines))
                      (line-end-position +dtrt-indent-tab-context-lines))
                     (dtrt-indent--analyze (dtrt-indent--calc-histogram lang))))))
           (modify-indentation (and (not (cdr (assoc :rejected result))) (cdr (assoc :change-indent-tabs-mode result))))
           (new-indent-tabs-mode (cdr (assoc :indent-tabs-mode-setting result))))
      (when (and modify-indentation (not (eq indent-tabs-mode new-indent-tabs-mode)))
        (+log! "Temporary changing `indent-tabs-mode' to %S for context-aware indentation" new-indent-tabs-mode))
      (let ((indent-tabs-mode (if modify-indentation new-indent-tabs-mode indent-tabs-mode)))
        (call-interactively #'tab-to-tab-stop)))))


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
           (when-let* ((new-color (+color-subtle 'isearch 5)))
             (set-face-background 'selection-highlight-mode-match-face new-color)
             (set-face-background 'selection-highlight-mode-alternate-match-face new-color))))))))


;; Minor mode for Emacs that deals with parens pairs and tries to be smart about it
(use-package smartparens
  :straight t
  :hook (minemacs-lazy . smartparens-global-mode)
  :custom
  (sp-ignore-modes-list '(minibuffer-inactive-mode)) ; Enable in `minibuffer-mode'
  :config
  (require 'smartparens-config)

  ;; In minibuffer, don't complete ' and `
  (sp-local-pair 'minibuffer-mode "'" nil :actions nil)
  (sp-local-pair 'minibuffer-mode "`" nil :actions nil)
  (sp-local-pair 'minibuffer-mode "`" "'" :when '(sp-in-string-p))
  (sp-local-pair 'org-mode "$" "$" :unless '(sp-point-after-word-p))

  ;; Silence some harmless but annoying echo-area spam
  (dolist (key '(:unmatched-expression :no-matching-tag)) (setf (alist-get key sp-message-alist) nil)))


;; Your friendly neighborhood expand-region clone
(use-package expreg
  :straight (:host github :repo "casouri/expreg")
  :when (+emacs-options-p 'tree-sitter)
  :bind (("C-M-SPC" . expreg-expand) ; orig. `mark-sexp'
         ("S-C-M-SPC" . expreg-contract)))


;; Drag stuff around in Emacs
(use-package drag-stuff
  :straight t
  :init
  :bind (("M-<up>" . drag-stuff-up)
         ("M-<down>" . drag-stuff-down)
         ("M-<left>" . drag-stuff-left)
         ("M-<right>" . drag-stuff-right)))


;; Collaborative editing using Conflict-free Replicated Data Types
(use-package crdt
  :straight t
  :when (or (executable-find "tuntox") (executable-find "stunnel"))
  :custom
  (crdt-tuntox-password-in-url t)
  (crdt-use-tuntox (executable-find "tuntox"))
  (crdt-use-stunnel (executable-find "stunnel")))


;; Perform a backup on each file save, real backup for Emacs!
(use-package real-backup
  :straight (:host github :repo "abougouffa/real-backup")
  :hook (minemacs-first-file . real-backup-mode))


(provide 'me-editor)

;;; me-editor.el ends here
