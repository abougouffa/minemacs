;;; me-nushell.el --- Nushell scripts -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

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
