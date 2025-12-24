;;; me-jenkins.el --- Jenkins file language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-22
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-jenkins
  :auto-mode '(("Jenkinsfile\\'" . jenkinsfile-mode)))


;; Major mode for editing Jenkins declarative pipeline syntax
(use-package jenkinsfile-mode
  :ensure t)


(provide 'on-demand/me-jenkins)
;;; me-jenkins.el ends here
