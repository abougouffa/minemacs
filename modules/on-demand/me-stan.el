;;; me-stan.el --- Stan language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-stan
  :auto-mode '(("\\.stan\\'" . stan-mode))
  :companion-packages '(((stan-mode stan-ts-mode) . (eldoc-stan stan-snippets))))


;; Major mode for editing Stan files
(use-package stan-mode
  :straight t)


;; Eldoc Eldoc support for Stan functions
(use-package eldoc-stan
  :straight t)


;; Yasnippets for Stan
(use-package stan-snippets
  :straight t)


(provide 'on-demand/me-stan)
;;; me-stan.el ends here
