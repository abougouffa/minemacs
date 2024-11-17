;;; me-rust.el --- Rust language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

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
  (rust-mode-treesitter-derive (+emacs-options-p 'tree-sitter)))


(provide 'on-demand/me-rust)
;;; me-rust.el ends here
