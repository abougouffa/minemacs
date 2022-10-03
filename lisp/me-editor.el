;; -*- lexical-binding: t; -*-

;; Visual Undo
(use-package vundo
  :straight t
  :general
  (me-global-def "ov" '(vundo :which-key "Visual Undo"))
  :config
  (setq vundo-compact-display t
        vundo-window-max-height 6
        vundo-glyph-alist
        '((selected-node   . ?●)
          (node            . ?○)
          (vertical-stem   . ?│)
          (branch          . ?├)
          (last-branch     . ?╰)
          (horizontal-stem . ?─))))


(use-package undo-fu
  :straight t
  :config
  (with-eval-after-load 'evil
    (evil-set-undo-system 'undo-fu)))


(use-package undo-fu-session
  :straight t
  :after undo-fu
  :config
  (setq undo-fu-session-compression 'zst
        undo-fu-session-directory (expand-file-name "undo-fu-session" minemacs-var-dir))
  (global-undo-fu-session-mode 1))


(provide 'me-editor)
