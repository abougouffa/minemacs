;;; me-undo.el --- Undo -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;; Visual Undo
(use-package vundo
  :straight t
  :init
  (+map! "ou" #'vundo)
  :custom
  (vundo-compact-display t)
  (vundo-window-max-height 8)
  (vundo-glyph-alist vundo-unicode-symbols))

(use-package undo-fu-session
  :straight t
  :init
  (undo-fu-session-global-mode 1)
  :custom
  (undo-fu-session-compression (if (executable-find "zstd") 'zst 'gz))
  (undo-fu-session-directory (concat minemacs-local-dir "undo-fu-session/")))


(provide 'me-undo)

;;; me-undo.el ends here
