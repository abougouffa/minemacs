;;; me-hy.el --- Hy language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-hy
  :auto-mode '(("\\.hy\\'" . hy-mode))
  :interpreter-mode '(("hy" . hy-mode))
  :companion-packages '((hy-mode . ob-hy)))

(use-package hy-mode
  :straight t)

(use-package ob-hy
  :straight t)


(provide 'on-demand/me-hy)
;;; me-hy.el ends here
