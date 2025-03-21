;;; me-pkgbuild.el --- PKGBUILD support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-pkgbuild
  :auto-mode '(("/PKGBUILD\\'" . pkgbuild-mode))
  :companion-packages '(((sh-mode bash-ts-mode) . pkgbuild-mode)))


;; Edit and run Arch Linux's PKGBUILD recipes
(use-package pkgbuild-mode
  :straight t)


(provide 'on-demand/me-pkgbuild)
;;; me-pkgbuild.el ends here
