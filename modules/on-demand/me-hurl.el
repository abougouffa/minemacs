;;; me-hurl.el --- Hurl support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-10
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-hurl
  :auto-mode '(("\\.hurl\\'" . hurl-mode)))


(minemacs-load-module 'on-demand/me-json)


;; Major mode to edit, run and test HTTP requests using Hurl
(use-package hurl-mode
  :vc (:url "https://github.com/JasZhe/hurl-mode")
  :mode "\\.hurl\\'")


(provide 'on-demand/me-hurl)
;;; me-hurl.el ends here
