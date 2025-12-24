;;; me-franca-idl.el --- Franca IDL -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-10
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-franca-idl
  :auto-mode '(("\\.fidl\\'" . franca-idl-mode)))


;; A major mode to edit Franca IDL code
(use-package franca-idl
  :vc (:url "https://github.com/zeph1e/franca-idl.el"))


(provide 'on-demand/me-franca-idl)
;;; me-franca-idl.el ends here
