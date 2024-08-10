;;; me-llvm.el --- LLVM specific syntax -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-extra-mode 'me-llvm 'llvm-ts-mode
  :auto-mode '(("\\.ll\\'" . llvm-ts-mode)))

(use-package llvm-ts-mode
  :straight t
  :when (+emacs-features-p 'tree-sitter))


(provide 'modes/me-llvm)
;;; me-llvm.el ends here
