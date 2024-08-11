;;; me-idris.el --- Idris language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-idris
  :auto-mode '(("\\.l?idr\\'" . idris-mode)))

(use-package idris-mode
  :straight t)


(provide 'on-demand/me-idris)
;;; me-idris.el ends here