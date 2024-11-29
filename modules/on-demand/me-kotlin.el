;;; me-kotlin.el --- Kotlin language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-kotlin
  :auto-mode '(("\\.kts?\\'" . (kotlin-mode kotlin-ts-mode))))


;; Major mode for the Kotlin programming language
(use-package kotlin-mode
  :ensure t)


;; Tree-sitter based major mode for the Kotlin programming language
(use-package kotlin-ts-mode
  :ensure t
  :when (+emacs-options-p 'tree-sitter)
  :mode "\\.kts?\\'")


(provide 'on-demand/me-kotlin)
;;; me-kotlin.el ends here
