;;; me-agda.el --- A -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-22
;; Last modified: 2026-11-21

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-agda
  :auto-mode '(("\\.l?agda\\'" . agda2-mode)))


;; Major mode for the Agda programming language
(use-package agda2-mode
  :ensure t)


(provide 'on-demand/me-agda)
;;; me-agda.el ends here
