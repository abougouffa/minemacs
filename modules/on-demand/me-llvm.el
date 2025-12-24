;;; me-llvm.el --- LLVM specific syntax -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-10
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-llvm
  :auto-mode '(("\\.ll\\'" . llvm-ts-mode))
  :companion-packages '(((llvm-mode llvm-ts-mode) . demangle-mode)))


;; LLVM major mode using Tree-sitter
(use-package llvm-ts-mode
  :ensure t
  :when (featurep 'feat/tree-sitter)
  :config
  (add-to-list 'treesit-language-source-alist '(llvm "https://github.com/benwilliamgraham/tree-sitter-llvm"))
  (treesit-ensure-installed 'llvm))


;; Automatically demangle C++, D, and Rust symbols in LLVM code
(use-package demangle-mode
  :ensure t)


(provide 'on-demand/me-llvm)
;;; me-llvm.el ends here
