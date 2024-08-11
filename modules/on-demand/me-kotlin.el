;;; me-kotlin.el --- Kotlin language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-kotlin
  :auto-mode '(("\\.kts?\\'" . (kotlin-mode kotlin-ts-mode))))

(use-package kotlin-mode
  :straight t)

(use-package kotlin-ts-mode
  :straight t
  :when (+emacs-features-p 'tree-sitter)
  :mode "\\.kts?\\'")


(provide 'on-demand/me-kotlin)
;;; me-kotlin.el ends here
