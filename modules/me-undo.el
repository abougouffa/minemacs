;;; me-undo.el --- Undo -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>

;; Visual Undo
(use-package vundo
  :straight t
  :defer t
  :general
  (+map "ou" #'vundo)
  :custom
  (vundo-compact-display t)
  (vundo-window-max-height 6)
  (vundo-glyph-alist
   '((selected-node   . ?●)
     (node            . ?○)
     (vertical-stem   . ?│)
     (branch          . ?├)
     (last-branch     . ?╰)
     (horizontal-stem . ?─))))

(use-package undo-fu
  :straight t
  :after minemacs-loaded
  :config
  (with-eval-after-load 'evil
    (evil-set-undo-system 'undo-fu)))

(use-package undo-fu-session
  :straight t
  :after undo-fu
  :custom
  (undo-fu-session-compression 'zst)
  (undo-fu-session-directory (concat minemacs-local-dir "undo-fu-session/"))
  :config
  (global-undo-fu-session-mode 1))


(provide 'me-undo)
