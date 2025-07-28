;;; me-awk.el --- More AWK stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-10
;; Last modified: 2025-07-28

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-awk
  :auto-mode '(("\\.awk\\'" . awk-ts-mode))
  :companion-packages '((awk-mode . awk-ts-mode)))


;; Major mode for AWK using Tree-sitter
(use-package awk-ts-mode
  :straight t
  :when (featurep 'feat/tree-sitter)
  :config
  (add-to-list 'treesit-language-source-alist '(awk "https://github.com/Beaglefoot/tree-sitter-awk"))
  (treesit-ensure-installed 'awk))


(provide 'on-demand/me-awk)
;;; me-awk.el ends here
