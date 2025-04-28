;;; me-ebuild.el --- ebuild language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-12
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-ebuild
  :auto-mode '(("\\.ebuild\\'" . ebuild-mode) ("\\.eclass\\'" . ebuild-eclass-mode)))


;; Major mode for editing Gentoo's ebuild and eclass files
(use-package ebuild-mode
  :straight t)


(provide 'on-demand/me-ebuild)
;;; me-ebuild.el ends here
