;;; me-idris.el --- Idris language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-11
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-idris
  :auto-mode '(("\\.l?idr\\'" . idris-mode)))


;; Major mode for the Idris programming language
(use-package idris-mode
  :ensure t)


(provide 'on-demand/me-idris)
;;; me-idris.el ends here
