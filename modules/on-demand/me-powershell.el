;;; me-powershell.el --- PowerShell -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-powershell
  :auto-mode '(("\\.ps[dm]?1\\'" . powershell-mode)))


;; Mode for editing PowerShell scripts
(use-package powershell
  :straight t)


(provide 'on-demand/me-powershell)
;;; me-powershell.el ends here
