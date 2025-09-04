;;; me-rust.el --- Rust language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-12
;; Last modified: 2025-09-04

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-rust
  :auto-mode '(("\\.rs\\'" . (rustic-mode rust-mode)))
  :companion-packages '((rust-ts-mode . (rust-mode rustic))))


;; Major mode for editing Rust source code
(use-package rust-mode
  :straight t
  :custom
  (rust-mode-treesitter-derive (featurep 'feat/tree-sitter)))


;; Rust development environment
(use-package rustic
  :straight t
  :custom
  (rustic-lsp-client 'eglot))


(provide 'on-demand/me-rust)
;;; me-rust.el ends here
