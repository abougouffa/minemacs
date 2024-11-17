;;; me-vala.el --- Vala language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-vala
  :auto-mode '(("\\.vala\\'" . vala-mode) ("\\.vapi\\'" . vala-mode))
  :companion-packages '((vala-mode . vala-snippets)))


;; Major mode for the Vala programming language
(use-package vala-mode
  :straight t)


;; Yasnippets for Vala
(use-package vala-snippets
  :straight t)


(provide 'on-demand/me-vala)
;;; me-vala.el ends here
