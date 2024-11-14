;;; me-llvm.el --- LLVM specific syntax -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-llvm
  :auto-mode '(("\\.ll\\'" . llvm-ts-mode)))

(use-package llvm-ts-mode
  :straight t
  :when (+emacs-options-p 'tree-sitter))


(provide 'on-demand/me-llvm)
;;; me-llvm.el ends here
