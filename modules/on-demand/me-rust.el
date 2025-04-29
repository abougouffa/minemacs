;;; me-rust.el --- Rust language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-12
;; Last modified: 2025-04-30

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-rust
  :auto-mode '(("\\.rs\\'" . rust-mode))
  :companion-packages '((rust-ts-mode . rust-mode)))


;; Major mode for editing Rust source code
(use-package rust-mode
  :straight t
  :custom
  (rust-mode-treesitter-derive (featurep 'feat/tree-sitter)))


(provide 'on-demand/me-rust)
;;; me-rust.el ends here
