;;; me-qsharp.el --- Q# language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-qsharp
  :auto-mode '(("\\.qs\\'" . qsharp-mode)))


;; Major mode for the Q# programming language
(use-package qsharp-mode
  :vc (:url "https://github.com/forked-from-1kasper/emacs-qsharp-mode"))


(provide 'on-demand/me-qsharp)
;;; me-qsharp.el ends here
