;;; me-selinux-policy.el --- SELinux policy definition files -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-selinux-policy
  :auto-mode '(("\\.te\\'" . selinuxpolicy-mode)))


;; Major mode for editing SELinux TE-RBAC
(use-package selinux-policy
  :straight (:host github :repo "pierre-rouleau/selinux-policy")
  :commands (selinuxpolicy-mode)
  :mode "\\.te\\'")


(provide 'on-demand/me-selinux-policy)
;;; me-selinux-policy.el ends here
