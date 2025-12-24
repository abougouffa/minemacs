;;; me-genexpr.el --- Support for GenExpr files -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-11-05
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-genexpr
  :auto-mode '(("\\.genexpr\\'" . genexpr-mode)))


;; Major mode for editing GenExpr files
(use-package genexpr-mode
  :ensure t)


(provide 'on-demand/me-genexpr)
;;; me-genexpr.el ends here
