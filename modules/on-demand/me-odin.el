;;; me-odin.el --- Odin language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-22
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-odin
  :auto-mode '(("\\.odin\\'" . odin-mode)))


;; Major mode for Odin
(use-package odin-mode
  :straight (:host github :repo "mattt-b/odin-mode"))


(provide 'on-demand/me-odin)
;;; me-odin.el ends here
