;;; me-hcl.el --- Hashicorp Configuration Language (HCL) -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-hcl
  :auto-mode '(("\\.hcl\\'" . hcl-mode) ("\\.nomad\\'" . hcl-mode) ("\\.t\\(f\\(vars\\)?\\|ofu\\)\\'" . terraform-mode)))

(use-package hcl-mode
  :straight t)

(use-package terraform-mode
  :straight t)


(provide 'on-demand/me-hcl)
;;; me-hcl.el ends here
