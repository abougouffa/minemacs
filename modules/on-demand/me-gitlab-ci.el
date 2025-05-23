;;; me-gitlab-ci.el --- Gitlab CI format support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-10
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-gitlab-ci
  :auto-mode '(("\\.gitlab-ci.ya?ml\\'" . gitlab-ci-mode))
  :companion-packages '(((yaml-mode yaml-ts-mode) . gitlab-ci-mode)))


;; Mode for editing GitLab CI files
(use-package gitlab-ci-mode
  :straight t)


(provide 'on-demand/me-gitlab-ci)
;;; me-gitlab-ci.el ends here
