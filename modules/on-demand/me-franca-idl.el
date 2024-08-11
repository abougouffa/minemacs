;;; me-franca-idl.el --- Franca IDL -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-extra-mode 'me-franca-idl 'franca-idl
  :auto-mode '(("\\.fidl\\'" . franca-idl-mode)))

(use-package franca-idl
  :straight (:host github :repo "zeph1e/franca-idl.el"))


(provide 'on-demand/me-franca-idl)
;;; me-franca-idl.el ends here
