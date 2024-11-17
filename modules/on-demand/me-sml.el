;;; me-sml.el --- Standard ML language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-sml
  :auto-mode '(("\\.s\\(?:ml\\|ig\\)\\'" . sml-mode)
               ("\\.cm\\'" . sml-cm-mode)
               ("\\.grm\\'" . sml-yacc-mode)))


;; Major mode for editing (Standard) ML
(use-package sml-mode
  :straight t
  :mode "\\.s\\(?:ml\\|ig\\)\\'"
  :config
  ;; Don't auto-close apostrophes and backticks
  (with-eval-after-load 'smartparens
    (sp-with-modes 'sml-mode
      (sp-local-pair "'" nil :actions nil)
      (sp-local-pair "`" nil :actions nil))))


(provide 'on-demand/me-sml)
;;; me-sml.el ends here
