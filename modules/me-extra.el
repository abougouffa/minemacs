;;; me-extra.el --- Some extra functionalities -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa and contributors

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(use-package better-jumper
  :straight t
  :hook (minemacs-after-startup . better-jumper-mode)
  ;; Map extra mouse buttons to jump forward/backward
  :bind (("<mouse-8>" . better-jumper-jump-backward)
         ("<mouse-9>" . better-jumper-jump-forward)))

(use-package crux
  :straight t
  :init
  (+map!
    "fo" #'crux-open-with
    "fC" #'crux-copy-file-preserve-attributes
    "id" #'crux-insert-date
    "bo" #'crux-kill-other-buffers))


(provide 'me-extra)

;;; me-extra.el ends here
