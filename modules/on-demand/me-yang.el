;;; me-yang.el --- YANG language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-yang
  :auto-mode '(("\\.[Yy][Aa][Nn][Gg]\\'" . yang-mode)))


;; Major mode for editing YANG files
(use-package yang-mode
  :straight t)


(provide 'on-demand/me-yang)
;;; me-yang.el ends here
