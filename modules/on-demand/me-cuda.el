;;; me-cuda.el --- CUDA integration -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-10
;; Last modified: 2025-09-04

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-cuda
  :auto-mode '(("\\.cu[h]?\\'" . cuda-ts-mode)))


;; Major mode for editing Nvidia CUDA C++ files
(use-package cuda-mode
  :straight t
  :unless (featurep 'feat/tree-sitter)
  :hook (cuda-mode . +prog-mode-run-hooks))


;; CUDA mode based on tree-sitter
(use-package cuda-ts-mode
  :straight (:host github :repo "Ergus/cuda-ts-mode")
  :when (featurep 'feat/tree-sitter)
  :config
  (add-to-list
   'treesit-language-source-alist
   '(cuda "https://github.com/tree-sitter-grammars/tree-sitter-cuda"))
  (treesit-ensure-installed 'cuda))


(provide 'on-demand/me-cuda)
;;; me-cuda.el ends here
