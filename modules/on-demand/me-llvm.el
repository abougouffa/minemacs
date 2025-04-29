;;; me-llvm.el --- LLVM specific syntax -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-10
;; Last modified: 2025-04-30

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-llvm
  :auto-mode '(("\\.ll\\'" . llvm-ts-mode)))


;; LLVM major mode using Tree-sitter
(use-package llvm-ts-mode
  :straight t
  :when (featurep 'feat/tree-sitter))


(provide 'on-demand/me-llvm)
;;; me-llvm.el ends here
