;;; me-scallop.el --- Scallop Language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-06-18
;; Last modified: 2025-06-18

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-scallop
  :auto-mode '(("\\.scl\\'" . scallop-mode)))


;; Major mode for editing Scallop programming language
(use-package scallop-mode
  :straight t)


(provide 'on-demand/me-scallop)
;;; me-scallop.el ends here
