;;; me-kmacro-x.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn@cneebg.pbz")
;; Created: 2025-08-14
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:


;; A collection of commands, modes and functions built on top of the keyboard macros (kmacros)
(use-package kmacro-x
  :ensure t
  :hook (minemacs-first-file . kmacro-x-atomic-undo-mode)
  ;; :bind (("C-<" . kmacro-x-mc-mark-previous)
  ;;        ("C->" . kmacro-x-mc-mark-next))
  :custom
  (kmacro-x-mc-live-preview t))


(provide 'obsolete/me-kmacro-x)
;;; me-kmacro-x.el ends here
