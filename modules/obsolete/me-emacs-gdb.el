;;; me-emacs-gdb.el --- Faster GDB integration for Emacs through a native module -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:


;; NOTE: This will overwrite the built-in `gdb-mi' for this session, maybe you
;; need to load this module only on-demand using "C-u M-x minemacs-load-module"

(use-package gdb-mi
  ;; I use my own fork in which I've merged some open PRs on the upstream.
  :straight `(:host github
              :repo "weirdNox/emacs-gdb" :files (:defaults "*.c" "*.h" "Makefile")
              :fork (:repo "abougouffa/emacs-gdb"))
  :demand
  :init
  (fmakunbound 'gdb)
  (fmakunbound 'gdb-enable-debug)
  :custom
  (gdb-window-setup-function #'gdb--setup-windows)
  (gdb-ignore-gdbinit nil))


(provide 'obsolete/me-emacs-gdb)
;;; me-emacs-gdb.el ends here
