;;; me-ebuild.el --- ebuild language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-ebuild
  :auto-mode '(("\\.ebuild\\'" . ebuild-mode) ("\\.eclass\\'" . ebuild-eclass-mode)))

(use-package ebuild-mode
  :straight t)


(provide 'on-demand/me-ebuild)
;;; me-ebuild.el ends here
