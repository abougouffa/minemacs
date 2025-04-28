;;; me-hy.el --- Hy language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-hy
  :auto-mode '(("\\.hy\\'" . hy-mode))
  :interpreter-mode '(("hy" . hy-mode))
  :companion-packages '((hy-mode . ob-hy)))


;; Major mode for the Hy programming language
(use-package hy-mode
  :straight t)


;; Org Babel code evaluation for the Hy language
(use-package ob-hy
  :straight t)


(provide 'on-demand/me-hy)
;;; me-hy.el ends here
