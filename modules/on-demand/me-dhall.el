;;; me-dhall.el --- Dhal configuration language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-dhall
  :auto-mode '(("\\.dhall\\'" . dhall-mode)))

(use-package dhall-mode
  :straight t)


(provide 'on-demand/me-dhall)
;;; me-dhall.el ends here
