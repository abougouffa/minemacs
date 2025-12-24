;;; me-experimental.el --- Experimental packages -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-12-11
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:


;; LSP Client for Emacs implemented as a module using Rust
(use-package lspce
  :vc ( :url "https://github.com/zbelial/lspce"
        :shell-command "cargo build --release && cp ./target/release/liblspce_module.so ./lspce-module.so")
  :when (and (featurep 'feat/modules) (not (featurep 'os/win)))
  :config
  (add-to-list
   'lspce-server-programs
   '("C/C++" "clangd" "--background-index" "-j=12" "--clang-tidy"
     "--all-scopes-completion" "--cross-file-rename" "--completion-style=detailed"
     "--header-insertion-decorators" "--header-insertion=iwyu" "--pch-storage=memory")))


(provide 'me-experimental)
;;; me-experimental.el ends here
