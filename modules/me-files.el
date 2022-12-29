;;; me-files.el --- File management -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


(use-package dirvish
  :straight t
  :hook (minemacs-after-startup . dirvish-override-dired-mode)
  :custom
  (dirvish-attributes '(subtree-state all-the-icons file-size vc-state git-msg))
  (dirvish-cache-dir (+directory-ensure (concat minemacs-cache-dir "dirvish/")))
  (dirvish-mode-line-format '(:left (sort file-time symlink) :right (omit yank index)))
  (dirvish-side-width 30)
  (dirvish-fd-default-dir "~/")
  (dirvish-use-header-line t) ; 'global make header line span all panes
  (dirvish-use-mode-line t)
  :general
  (+map
    ;; Open
    "o-" '(dirvish :wk "Dirvish")
    "op" '(dirvish-side :wk "Side panel")
    "oq" '(dirvish-quick-access :wk "Dirvish quick access")
    ;; Search
    "sd" '(dirvish-fd :wk "Dirvish fd"))
  (+map-key :keymaps 'dirvish-mode-map
    "q" #'dirvish-quit
    "s" #'dirvish-subtree-toggle
    "y" #'dirvish-yank-menu))

(use-package vlf-setup
  :straight vlf
  :after minemacs-loaded)


(provide 'me-files)
