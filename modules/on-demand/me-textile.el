;;; me-textile.el --- Textile markup language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-textile
  :auto-mode '(("\\.textile\\'" . textile-mode)))


;; Textile markup editing major mode
(use-package textile-mode
  :straight t
  :mode "\\.textile\\'")


(provide 'on-demand/me-textile)
;;; me-textile.el ends here
