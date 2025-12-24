;;; me-selinux-policy.el --- SELinux policy definition files -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-11-03
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-selinux-policy
  :auto-mode '(("\\.te\\'" . selinuxpolicy-mode)))


;; Major mode for editing SELinux TE-RBAC
(use-package selinux-policy
  :vc (:url "https://github.com/pierre-rouleau/selinux-policy")
  :commands (selinuxpolicy-mode)
  :mode "\\.te\\'")


(provide 'on-demand/me-selinux-policy)
;;; me-selinux-policy.el ends here
