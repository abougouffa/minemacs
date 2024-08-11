;;; me-hurl.el --- Hurl support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-extra-mode 'me-hurl 'hurl-mode
  :auto-mode '(("\\.hurl\\'" . hurl-mode)))

(use-package hurl-mode
  :straight (:host github :repo "JasZhe/hurl-mode")
  :mode "\\.hurl\\'")


(provide 'on-demand/me-hurl)
;;; me-hurl.el ends here
