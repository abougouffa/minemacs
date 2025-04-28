;;; me-nushell.el --- Nushell scripts -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-11-30
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-nushell
  :auto-mode '(("\\.nu\\'" . nushell-mode))
  :interpreter-mode '(("nu" . nushell-mode)))


(use-package nushell-mode
  :straight t)


(provide 'on-demand/me-nushell)
;;; me-nushell.el ends here
