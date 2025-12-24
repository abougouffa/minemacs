;;; me-ballerina.el --- Ballerina language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-22
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-ballerina
  :auto-mode '(("\\.bal\\'" . ballerina-mode)))


;; A major mode for editing ballerina source code
(use-package ballerina-mode
  :vc (:url "https://github.com/heshanpadmasiri/ballerina-mode")
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(ballerina-mode . ("bal" "start-language-server")))))


(provide 'on-demand/me-ballerina)
;;; me-ballerina.el ends here
