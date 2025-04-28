;;; me-agda.el --- A -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-agda
  :auto-mode '(("\\.l?agda\\'" . agda2-mode)))


;; Major mode for the Agda programming language
(use-package agda2-mode
  :straight t)


(provide 'on-demand/me-agda)
;;; me-agda.el ends here
