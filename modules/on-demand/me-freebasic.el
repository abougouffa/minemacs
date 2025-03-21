;;; me-freebasic.el --- FreeBasic support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-freebasic
  :auto-mode '(("\\.b\\(i\\|as\\)\\'" . fb-mode)))


;; A major mode for the FreeBASIC programming language
(use-package fb-mode
  :straight (:host github :repo "rversteegen/fb-mode")
  :commands (fb-mode)
  :mode "\\.b\\(i\\|as\\)\\'")


(provide 'on-demand/me-freebasic)
;;; me-freebasic.el ends here
