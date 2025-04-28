;;; me-stan.el --- Stan language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-12
;; Last modified: 2025-03-21

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
