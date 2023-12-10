;;; me-undo.el --- Undo -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa and contributors

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

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
  :hook (minemacs-after-startup . global-undo-fu-session-mode)
  :demand t
  :custom
  (undo-fu-session-compression (if (executable-find "zstd") 'zst 'gz))
  (undo-fu-session-directory (concat minemacs-local-dir "undo-fu-session/")))


(provide 'me-undo)

;;; me-undo.el ends here
