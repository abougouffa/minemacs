;;; me-files.el --- File management -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2022-10-04
;; Last modified: 2025-08-26

;;; Commentary:

;;; Code:

;; Collection of useful dired additions
(use-package dired-hacks
  :straight t
  :after dired
  :bind
  ( :map dired-mode-map
    ([tab] . dired-subtree-toggle)
    ([backtab] . dired-subtree-remove))
  :custom
  (dired-subtree-use-backgrounds nil))


;; Asynchronous "rsync" from `dired'
(use-package dired-rsync
  :straight (:files ("dired-rsync.el" "dired-rsync-transient.el"))
  :bind ( :package dired :map dired-mode-map
          ("C-c C-r" . dired-rsync)
          ("C-c C-x" . dired-rsync-transient)))


;; Viewing and editing system trash can
(use-package trashed
  :straight t)


;; Sort and browse disk usage listings
(use-package disk-usage
  :straight t)


;; Fast opening of large files
(use-package guard-lf
  :straight (:host github :repo "jcs-elpa/guard-lf")
  :init
  (guard-lf-mode 1))


;; Utilities for opening files with "sudo"
(use-package sudo-edit
  :straight t
  :hook (minemacs-first-file . sudo-edit-indicator-mode))


;; Directory tree comparison mode for Emacs (inspired by commercial tools like Beyond Compare and Araxis Merge)
(use-package ztree
  :straight t
  :bind (:map minemacs-open-thing-map ("z" . ztree-diff))
  :init
  (with-eval-after-load 'ztree-view
    (setq ztree-draw-unicode-lines t)
    (keymap-set ztree-mode-map "n" #'ztree-next-line)
    (keymap-set ztree-mode-map "p" #'ztree-previous-line)))


;; Apply all (!) ".dir-locals.el" from root to current directory
(use-package cascading-dir-locals
  :straight t
  :custom
  (cascading-dir-locals-debug minemacs-debug-p))


(provide 'me-files)

;;; me-files.el ends here
