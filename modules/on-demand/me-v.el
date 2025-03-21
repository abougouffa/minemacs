;;; me-v.el --- V language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-v
  :auto-mode '(("\\(\\.v?v\\|\\.vsh\\)$" . v-mode)))


;; Major mode for the V programming language
(use-package v-mode
  :straight t)


(provide 'on-demand/me-v)
;;; me-v.el ends here
