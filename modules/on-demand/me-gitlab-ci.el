;;; me-gitlab-ci.el --- Gitlab CI format support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-gitlab-ci
  :auto-mode '(("\\.gitlab-ci.ya?ml\\'" . gitlab-ci-mode))
  :companion-packages '(((yaml-mode yaml-ts-mode) . gitlab-ci-mode)))


;;;###autoload(autoload '+gitlab-ci-add-schema "on-demand/me-gitlab-ci")


;; Mode for editing GitLab CI files
(use-package gitlab-ci-mode
  :straight t
  :init
  (defun +gitlab-ci-add-schema ()
    (interactive)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (insert "# yaml-language-server: "
                "$schema=https://gitlab.com/gitlab-org/gitlab/-/raw/master/app/assets/javascripts/editor/schema/ci.json"
                "\n\n")))))


(provide 'on-demand/me-gitlab-ci)
;;; me-gitlab-ci.el ends here
