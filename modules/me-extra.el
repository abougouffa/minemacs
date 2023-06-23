;;; me-extra.el --- Some extra functionalities -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(use-package better-jumper
  :straight t
  :hook (minemacs-after-startup . better-jumper-mode)
  :config
  ;; Map extra mouse buttons to jump forward/backward
  (keymap-global-set "<mouse-8>" 'better-jumper-jump-backward)
  (keymap-global-set "<mouse-9>" 'better-jumper-jump-forward))

(use-package crux
  :straight t
  :init
  (+map!
    "fo" #'crux-open-with
    "fU" #'crux-sudo-edit
    "fD" #'crux-delete-file-and-buffer
    "fC" #'crux-copy-file-preserve-attributes
    "id" #'crux-insert-date
    "bo" #'crux-kill-other-buffers))


(provide 'me-extra)

;;; me-extra.el ends here
