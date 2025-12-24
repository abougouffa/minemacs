;;; me-kotlin.el --- Kotlin language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-11
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-kotlin
  :auto-mode '(("\\.kts?\\'" . (kotlin-mode kotlin-ts-mode))))


;; Major mode for the Kotlin programming language
(use-package kotlin-mode
  :ensure t
  :unless (featurep 'feat/tree-sitter))


;; Tree-sitter based major mode for the Kotlin programming language
(use-package kotlin-ts-mode
  :ensure t
  :when (featurep 'feat/tree-sitter)
  :mode "\\.kts?\\'"
  :config
  (add-to-list 'treesit-language-source-alist '(kotlin "https://github.com/fwcd/tree-sitter-kotlin"))
  (treesit-ensure-installed 'kotlin))


(provide 'on-demand/me-kotlin)
;;; me-kotlin.el ends here
