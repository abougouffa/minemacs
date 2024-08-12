;;; me-rust.el --- Rust language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn-rkg@fntrzpbz.pbz")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-rust
  :auto-mode '(("\\.rs\\'" . rust-mode))
  :companion-packages '((rust-ts-mode . rust-mode)))

(use-package rust-mode
  :straight t
  :custom
  (rust-mode-treesitter-derive (+emacs-features-p 'tree-sitter)))


(provide 'on-demand/me-rust)
;;; me-rust.el ends here