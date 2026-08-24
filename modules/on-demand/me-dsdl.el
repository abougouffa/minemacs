;;; me-dsdl.el --- Data Structure Description Language (DSDL) -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-11
;; Last modified: 2026-08-24

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-dsdl
  :auto-mode '(("\\.dsdl\\'" . dsdl-mode) ("\\.uavcan\\'" . dsdl-mode)))


;; Major mode for the Data Structure Description Language (DSDL)
(use-package dsdl-mode
  :straight (:host github :repo "abougouffa/dsdl-mode"))


(provide 'on-demand/me-dsdl)
;;; me-dsdl.el ends here
