;;; me-yang.el --- YANG language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn-rkg@fntrzpbz.pbz")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-yang
  :auto-mode '(("\\.[Yy][Aa][Nn][Gg]\\'" . yang-mode)))

(use-package yang-mode
  :straight t)


(provide 'on-demand/me-yang)
;;; me-yang.el ends here
