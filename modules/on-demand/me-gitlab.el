;;; me-gitlab.el --- Gitlab CI format support and interface -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-10
;; Last modified: 2026-11-20

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-gitlab
  :auto-mode '(("\\.gitlab-ci.ya?ml\\'" . gitlab-ci-mode))
  :companion-packages '(((yaml-mode yaml-ts-mode) . gitlab-ci-mode))
  :define-loader t)


;; Mode for editing GitLab CI files
(use-package gitlab-ci-mode
  :ensure t)


;; Emacs-GitLab integration
(use-package lab
  :ensure t)


(provide 'on-demand/me-gitlab)
;;; me-gitlab.el ends here
