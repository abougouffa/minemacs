;;; me-vb.el --- Visual Basic language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-vb
  :auto-mode '(("\\.vbs?\\'" . visual-basic-mode)))

(use-package visual-basic-mode
  :straight t
  :mode "\\.vbs?\\'")


(provide 'on-demand/me-vb)
;;; me-vb.el ends here
