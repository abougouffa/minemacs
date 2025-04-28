;;; me-qsharp.el --- Q# language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-23
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-qsharp
  :auto-mode '(("\\.qs\\'" . qsharp-mode)))


;; Major mode for the Q# programming language
(use-package qsharp-mode
  :straight (:host github :repo "forked-from-1kasper/emacs-qsharp-mode"))


(provide 'on-demand/me-qsharp)
;;; me-qsharp.el ends here
