;;; me-d.el --- D language support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-11
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-d
  :auto-mode '(("\\.d[i]?\\'" . d-mode)))


;; Major mode for the D programming language
(use-package d-mode
  :ensure t)


(provide 'on-demand/me-d)
;;; me-d.el ends here
