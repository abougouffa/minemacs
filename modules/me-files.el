;;; me-files.el --- File management -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


(use-package dirvish
  :straight t
  :defer t
  :custom
  (dirvish-attributes '(file-size vc-state git-msg all-the-icons))
  (dirvish-cache-dir (concat minemacs-cache-dir "dirvish/"))
  (dirvish-mode-line-format '(:left (sort file-time symlink) :right (omit yank index)))
  (dirvish-side-width 30)
  (dirvish-fd-default-dir "~/")
  :general
  (+map
    ;; Open
    "o-"  '(dirvish :wk "Dirvish")
    "op"  '(dirvish-side :wk "Side panel")
    "oq"  '(dirvish-quick-access :wk "Dirvish quick access")
    ;; Search
    "sd"  '(dirvish-fd :wk "Dirvish fd"))
  (+map-key :keymaps 'dirvish-mode-map
    "q" #'dirvish-quit)
  :config
  (require 'dirvish-fd)
  (require 'dirvish-yank)
  (require 'dirvish-icons)
  (require 'dirvish-emerge)
  (dirvish-override-dired-mode))

(use-package vlf-setup
  :straight (vlf)
  :after minemacs-loaded)


(provide 'me-files)
