;;; me-p4.el --- P4 language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-p4
  :auto-mode '(("\\.p4\\(info\\)?\\'" . p4-16-mode)))


;; Support for the P4_16 programming language
(use-package p4-16-mode
  :straight t
  :mode "\\.p4\\'")


(provide 'on-demand/me-p4)
;;; me-p4.el ends here
