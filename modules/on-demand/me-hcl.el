;;; me-hcl.el --- Hashicorp Configuration Language (HCL) -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-23
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-hcl
  :auto-mode '(("\\.hcl\\'" . hcl-mode) ("\\.nomad\\'" . hcl-mode) ("\\.t\\(f\\(vars\\)?\\|ofu\\)\\'" . terraform-mode)))


;; Major mode for Hashicorp Configuration Language (HCL)
(use-package hcl-mode
  :ensure t)


;; Major mode for Terraform configuration files
(use-package terraform-mode
  :ensure t)


(provide 'on-demand/me-hcl)
;;; me-hcl.el ends here
