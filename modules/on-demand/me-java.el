;;; me-java.el --- Java language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-java
  :auto-mode '((("\\.g\\(?:ant\\|roovy\\|radle\\)\\'" "/Jenkinsfile\\'") . groovy-mode))
  :interpreter-mode '(("groovy" . groovy-mode))
  :companion-packages '(((java-mode java-ts-mode) . (groovy-mode android-mode))))

(use-package groovy-mode
  :straight t)

(use-package android-mode
  :straight t)


(provide 'on-demand/me-java)
;;; me-java.el ends here