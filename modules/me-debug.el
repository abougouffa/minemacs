;;; me-debug.el --- Debugging stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2022-12-15
;; Last modified: 2025-06-14

;;; Commentary:

;;; Code:

;; Debug Adapter Protocol for Emacs
(use-package dape
  :straight t
  :hook
  (dape-compile . kill-buffer) ; Kill compile buffer on build success
  (dape-display-source . pulse-momentary-highlight-one-line) ; Pulse source line (performance hit)
  (dape-stopped . dape-info) ; To display info and/or repl buffers on stopped
  (dape-stopped . dape-repl)
  (dape-start . (lambda () (project-save-some-buffers t))) ; Save buffers on startup, useful for interpreted languages
  :config
  (dape-breakpoint-load) ; Load breakpoints on startup, with laziness
  (add-hook 'kill-emacs-hook #'dape-breakpoint-save)) ; Save breakpoints on quit


;; A compiler output viewer
(use-package rmsbolt
  :straight t
  :hook (rmsbolt-mode . +rmsbolt-disable-flymake-in-asm)
  :config
  (defun +rmsbolt-disable-flymake-in-asm ()
    (when (derived-mode-p 'asm-mode) (flymake-mode-off))))


;; Compiler Explorer clone (fork of `rmsbolt' optimized for C/C++)
(use-package beardbolt
  :straight (:host github :repo "joaotavora/beardbolt" :files (:defaults "starters"))
  :hook (beardbolt--asm-mode . flymake-mode-off)
  :config
  (add-to-list 'beardbolt-languages '(rust-ts-mode beardbolt--rust-setup)))


;; Use "objdump" to display disassembled executable and object files
(use-package objdump-disassemble
  :straight (:host github :repo "abougouffa/objdump-disassemble")
  :hook (minemacs-first-file . global-objdump-disassemble-mode))


(provide 'me-debug)

;;; me-debug.el ends here
