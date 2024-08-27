;;; me-mojo.el --- Mojo language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-mojo
  :auto-mode '(("\\.mojo\\'" . mojo-mode) ("\\.🔥\\'" . mojo-mode)))

(use-package mojo
  :straight (:host github :repo "andcarnivorous/mojo-hl")
  :commands (mojo-mode mojo-compile)
  :mode ("\\.🔥\\'" . mojo-mode))


(provide 'on-demand/me-mojo)
;;; me-mojo.el ends here
