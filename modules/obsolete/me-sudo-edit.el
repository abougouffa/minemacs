;;; me-sudo-edit.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn@cneebg.pbz")
;; Created: 2025-08-26
;; Last modified: 2026-11-21

;;; Commentary:

;;; Code:


;; Utilities for opening files with "sudo"
(use-package sudo-edit
  :ensure t
  :hook (minemacs-first-file . sudo-edit-indicator-mode))


(provide 'obsolete/me-sudo-edit)
;;; me-sudo-edit.el ends here
