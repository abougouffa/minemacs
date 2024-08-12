;;; me-v.el --- V language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn-rkg@fntrzpbz.pbz")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-v
  :auto-mode '(("\\(\\.v?v\\|\\.vsh\\)$" . v-mode)))

(use-package v-mode
  :straight t)


(provide 'on-demand/me-v)
;;; me-v.el ends here
