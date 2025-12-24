;;; me-v.el --- V language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-12
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-v
  :auto-mode '(("\\(\\.v?v\\|\\.vsh\\)$" . v-mode)))


;; Major mode for the V programming language
(use-package v-mode
  :ensure t)


(provide 'on-demand/me-v)
;;; me-v.el ends here
