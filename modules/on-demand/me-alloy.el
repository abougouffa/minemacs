;;; me-alloy.el --- Alloy -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-alloy
  :auto-mode '(("\\.als\\'" . alloy-mode)))


;; Emacs major mode for Alloy language
(use-package alloy-mode
  :vc (:url "https://github.com/dwwmmn/alloy-mode")
  :mode "\\.als\\'")


(provide 'on-demand/me-alloy)
;;; me-alloy.el ends here
