;;; me-kotlin.el --- Kotlin language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-kotlin
  :auto-mode '(("\\.kts?\\'" . (kotlin-mode kotlin-ts-mode))))


;; Major mode for the Kotlin programming language
(use-package kotlin-mode
  :straight t)


;; Tree-sitter based major mode for the Kotlin programming language
(use-package kotlin-ts-mode
  :straight t
  :when (+emacs-options-p 'tree-sitter)
  :mode "\\.kts?\\'")


(provide 'on-demand/me-kotlin)
;;; me-kotlin.el ends here
