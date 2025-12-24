;;; me-yang.el --- YANG language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-12
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-yang
  :auto-mode '(("\\.[Yy][Aa][Nn][Gg]\\'" . yang-mode)))


;; Major mode for editing YANG files
(use-package yang-mode
  :ensure t)


(provide 'on-demand/me-yang)
;;; me-yang.el ends here
