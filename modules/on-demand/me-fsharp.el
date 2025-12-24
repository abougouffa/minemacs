;;; me-fsharp.el --- F# language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-11
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-fsharp
  :auto-mode '(("\\.fs[iylx]?\\'" . fsharp-mode) ("\\.fsproj\\'" . nxml-mode)))


;; Support for the F# programming language
(use-package fsharp-mode
  :ensure t)


(provide 'on-demand/me-fsharp)
;;; me-fsharp.el ends here
