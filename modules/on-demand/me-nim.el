;;; me-nim.el --- Nim language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-11
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-nim
  :auto-mode '(("\\.nim\\'" . nim-mode) ("\\.nim\\(ble\\|s\\)\\'" . nimscript-mode-maybe)))


;; A major mode for the Nim programming language
(use-package nim-mode
  :ensure t)


(provide 'on-demand/me-nim)
;;; me-nim.el ends here
