;;; me-sysml.el --- Systems Modeling Language (SysML v2) -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn@tznvy.pbz")
;; Created: 2026-06-04
;; Last modified: 2026-06-04

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-sysml
  :auto-mode '(("\\.sysml\\'" . sysml-mode)))


;; Major mode for SysML v2 (Systems Modeling Language)
(use-package sysml-mode
  :straight t)


(provide 'on-demand/me-sysml)
;;; me-sysml.el ends here
