;;; me-cython.el --- Cython language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-17
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-cython
  :auto-mode '((("\\.pyx\\'" "\\.px[di]\\'") . cython-mode)))


;; Major mode for editing Cython files
(use-package cython-mode
  :ensure t)


(provide 'on-demand/me-cython)
;;; me-cython.el ends here
