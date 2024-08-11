;;; me-nim.el --- Nim language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-nim
  :auto-mode '(("\\.nim\\'" . nim-mode) ("\\.nim\\(ble\\|s\\)\\'" . nimscript-mode-maybe)))

(push 'flycheck-nimsuggest straight-built-in-pseudo-packages)

(use-package nim-mode
  :straight t)

(cl-callf2 remq 'flycheck-nimsuggest straight-built-in-pseudo-packages)


(provide 'on-demand/me-nim)
;;; me-nim.el ends here
