;;; me-pkgbuild.el --- PKGBUILD support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-10
;; Last modified: 2026-07-11

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-pkgbuild
  :auto-mode '(("/PKGBUILD\\'" . pkgbuild-mode))
  :companion-packages '(((sh-mode bash-ts-mode) . pkgbuild-mode)))


;; Edit and run Arch Linux's PKGBUILD recipes
(use-package pkgbuild-mode
  :straight t
  :custom
  (pkgbuild-update-sums-on-save nil)) ; Annoying, slow and buggy in some cases


(provide 'on-demand/me-pkgbuild)
;;; me-pkgbuild.el ends here
