;;; me-dhall.el --- Dhal configuration language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-12
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-dhall
  :auto-mode '(("\\.dhall\\'" . dhall-mode)))


;; Major mode for the Dhall configuration language
(use-package dhall-mode
  :straight t)


(provide 'on-demand/me-dhall)
;;; me-dhall.el ends here
