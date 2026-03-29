;;; me-cpio.el --- CPIO archives -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2026-03-29
;; Last modified: 2026-03-29

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-cpio
  :auto-mode '(("\\.cpio\\'" . cpio-mode)))


;; Handle cpio archives in the style of dired
(use-package cpio-mode
  :straight t
  :mode "\\.cpio\\'")


(provide 'on-demand/me-cpio)
;;; me-cpio.el ends here
