;;; me-ballerina.el --- Ballerina language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-ballerina
  :auto-mode '(("\\.bal\\'" . ballerina-mode)))


;; A major mode for editing ballerina source code
(use-package ballerina-mode
  :straight (:host github :repo "heshanpadmasiri/ballerina-mode")
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(ballerina-mode . ("bal" "start-language-server")))))


(provide 'on-demand/me-ballerina)
;;; me-ballerina.el ends here
