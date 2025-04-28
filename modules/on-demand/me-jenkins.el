;;; me-jenkins.el --- Jenkins file language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-jenkins
  :auto-mode '(("Jenkinsfile\\'" . jenkinsfile-mode)))


;; Major mode for editing Jenkins declarative pipeline syntax
(use-package jenkinsfile-mode
  :straight t)


(provide 'on-demand/me-jenkins)
;;; me-jenkins.el ends here
