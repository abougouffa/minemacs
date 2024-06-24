;;; me-undo.el --- Undo -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;; Visual Undo
(use-package vundo
  :straight t
  :custom
  (vundo-compact-display t)
  (vundo-window-max-height 8)
  (vundo-glyph-alist vundo-unicode-symbols))

(use-package undo-fu-session
  :straight t
  :hook (minemacs-lazy . undo-fu-session-global-mode)
  :custom
  (undo-fu-session-compression (if (executable-find "zstd") 'zst 'gz)))


(provide 'me-undo)

;;; me-undo.el ends here
