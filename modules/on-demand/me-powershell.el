;;; me-powershell.el --- PowerShell -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Last modified: 2025-04-17

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-powershell
  :auto-mode '(("\\.ps[dm]?1\\'" . powershell-mode)))


;; Mode for editing PowerShell scripts
(use-package powershell
  :straight t
  :custom
  (powershell-default-langserver-path (expand-file-name "powershell/" minemacs-local-dir)))


(provide 'on-demand/me-powershell)
;;; me-powershell.el ends here
