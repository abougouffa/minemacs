;;; me-alloy.el --- Alloy -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-11
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-alloy
  :auto-mode '(("\\.als\\'" . alloy-mode)))


;; Emacs major mode for Alloy language
(use-package alloy-mode
  :straight (:host github :repo "dwwmmn/alloy-mode")
  :mode "\\.als\\'")


(provide 'on-demand/me-alloy)
;;; me-alloy.el ends here
