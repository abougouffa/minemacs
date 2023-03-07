;;; me-extra.el --- Some extra functionalities -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


(use-package better-jumper
  :straight t
  :hook (minemacs-after-startup . better-jumper-mode)
  :config
  ;; Map extra mouse buttons to jump forward/backward
  (global-set-key [mouse-8] #'better-jumper-jump-backward)
  (global-set-key [mouse-9] #'better-jumper-jump-forward))

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
