;;; me-genexpr.el --- Support for GenExpr files -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-genexpr
  :auto-mode '(("\\.genexpr\\'" . genexpr-mode)))

(use-package genexpr-mode
  :straight t)


(provide 'on-demand/me-genexpr)
;;; me-genexpr.el ends here