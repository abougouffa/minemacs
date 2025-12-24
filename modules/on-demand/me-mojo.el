;;; me-mojo.el --- Mojo language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-27
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-mojo
  :auto-mode '(("\\.mojo\\'" . mojo-mode) ("\\.🔥\\'" . mojo-mode)))


;; Major mode for the Mojo programming lanugage
(use-package mojo
  :vc (:url "https://github.com/andcarnivorous/mojo-hl")
  :commands (mojo-mode mojo-compile)
  :mode ("\\.🔥\\'" . mojo-mode))


(provide 'on-demand/me-mojo)
;;; me-mojo.el ends here
