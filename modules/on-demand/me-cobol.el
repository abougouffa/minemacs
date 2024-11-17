;;; me-cobol.el --- Cobol language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-cobol
  :auto-mode '(("\\.c\\(ob\\|bl\\|py\\)\\'" . cobol-mode)))


;; Major mode for editing COBOL code
(use-package cobol-mode
  :straight t)


(provide 'on-demand/me-cobol)
;;; me-cobol.el ends here
