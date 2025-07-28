;;; me-experimental.el --- Experimental packages -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-12-11
;; Last modified: 2025-07-28

;;; Commentary:

;;; Code:


;; A collection of commands, modes and functions built on top of the keyboard macros (kmacros)
(use-package kmacro-x
  :straight t
  :hook (minemacs-first-file . kmacro-x-atomic-undo-mode)
  ;; :bind (("C-<" . kmacro-x-mc-mark-previous)
  ;;        ("C->" . kmacro-x-mc-mark-next))
  :custom
  (kmacro-x-mc-live-preview t))


;; LSP Client for Emacs implemented as a module using Rust
(use-package lspce
  :straight `( :host github :repo "zbelial/lspce"
               :files (:defaults ,(file-name-with-extension "lspce-module" module-file-suffix))
               :pre-build (("cargo" "build" "--release")
                           ("cp"
                            ,(file-name-with-extension "./target/release/liblspce_module" module-file-suffix)
                            ,(file-name-with-extension "./lspce-module" module-file-suffix))))
  :when (and (featurep 'feat/modules) (executable-find "cargo"))
  :config
  (add-to-list
   'lspce-server-programs
   '("C/C++" "clangd" "--background-index" "-j=12" "--clang-tidy"
     "--all-scopes-completion" "--cross-file-rename" "--completion-style=detailed"
     "--header-insertion-decorators" "--header-insertion=iwyu" "--pch-storage=memory")))


(provide 'me-experimental)
;;; me-experimental.el ends here
