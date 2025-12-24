;;; me-eglot-inactive-regions.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-06-11
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:


;; Highlight inactive code regions with eglot power (mainly C/C++ preprocessor directives)
(use-package eglot-inactive-regions
  :ensure t
  :commands (eglot-inactive-regions-mode))


(provide 'obsolete/me-eglot-inactive-regions)
;;; me-eglot-inactive-regions.el ends here
