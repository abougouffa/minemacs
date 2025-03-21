;;; me-hurl.el --- Hurl support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-hurl
  :auto-mode '(("\\.hurl\\'" . hurl-mode)))


(minemacs-load-module 'on-demand/me-json)


;; Major mode to edit, run and test HTTP requests using Hurl
(use-package hurl-mode
  :straight (:host github :repo "JasZhe/hurl-mode")
  :mode "\\.hurl\\'")


(provide 'on-demand/me-hurl)
;;; me-hurl.el ends here
