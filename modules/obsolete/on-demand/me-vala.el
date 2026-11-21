;;; me-vala.el --- Vala language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-12
;; Last modified: 2026-11-21

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-vala
  :auto-mode '(("\\.vala\\'" . vala-mode) ("\\.vapi\\'" . vala-mode))
  :companion-packages '((vala-mode . vala-snippets)))


;; Major mode for the Vala programming language
(use-package vala-mode
  :ensure t)


;; Yasnippets for Vala
(use-package vala-snippets
  :ensure t)


(provide 'on-demand/me-vala)
;;; me-vala.el ends here
