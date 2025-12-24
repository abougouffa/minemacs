;;; me-files.el --- File management -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2022-10-04
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;; Collection of useful dired additions
(use-package dired-hacks
  :vc (:url "https://github.com/Fuco1/dired-hacks")
  :after dired
  :bind
  ( :map dired-mode-map
    ([tab] . dired-subtree-toggle)
    ([backtab] . dired-subtree-remove))
  :custom
  (dired-subtree-use-backgrounds nil))


;; Asynchronous "rsync" from `dired'
(use-package dired-rsync
  :ensure t
  :bind ( :package dired :map dired-mode-map
          ("C-c C-r" . dired-rsync)
          ("C-c C-x" . dired-rsync-transient)))


;; Same functionality as `find-dired' and `find-grep-dired', using fd/rg instead
(use-package fd-dired
  :ensure t)


;; Viewing and editing system trash can
(use-package trashed
  :ensure t
  :bind (:map minemacs-open-thing-map ("T" . trashed)))


;; Sort and browse disk usage listings
(use-package disk-usage
  :ensure t)


;; View, edit, search and compare very large files in batches, trading memory for processor time
(use-package vlf-setup
  :ensure vlf
  :pin gnu
  :demand
  :config
  (with-eval-after-load 'so-long
    (add-to-list 'so-long-mode-preserved-variables 'vlf-mode)))


;; Fast opening of large files
(use-package guard-lf
  :vc (:url "https://github.com/jcs-elpa/guard-lf")
  :init
  (guard-lf-mode 1))


;; Directory tree comparison mode for Emacs (inspired by commercial tools like Beyond Compare and Araxis Merge)
(use-package ztree
  :ensure t
  :bind (:map minemacs-open-thing-map ("z" . ztree-diff))
  :init
  (with-eval-after-load 'ztree-view
    (setq ztree-draw-unicode-lines t)
    (keymap-set ztree-mode-map "n" #'ztree-next-line)
    (keymap-set ztree-mode-map "p" #'ztree-previous-line)))


;; Apply all (!) ".dir-locals.el" from root to current directory
(use-package cascading-dir-locals
  :ensure t
  :custom
  (cascading-dir-locals-debug minemacs-debug-p))


(provide 'me-files)

;;; me-files.el ends here
