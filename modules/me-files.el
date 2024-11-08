;;; me-files.el --- File management -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;; Collection of useful dired additions
(use-package dired-hacks
  :straight t)


;; Sort and browse disk usage listings
(use-package disk-usage
  :straight t)


;; A Emacs tree plugin like NerdTree for Vim
(use-package neotree
  :straight t
  :custom
  (neo-theme 'nerd-icons))


;; Same frame speedbar
(use-package sr-speedbar
  :straight t)


;; View, edit, search and compare very large files in batches, trading memory for processor time
(use-package vlf-setup
  :straight (vlf :source gnu-elpa-mirror)
  :demand
  :config
  (with-eval-after-load 'so-long
    (add-to-list 'so-long-mode-preserved-variables 'vlf-mode)))


;; Fast opening of large files
(use-package guard-lf
  :straight (:host github :repo "jcs-elpa/guard-lf")
  :custom
  (guard-lf-major-mode #'guard-lf-large-file-mode)
  :init
  (guard-lf-mode 1)
  (define-derived-mode guard-lf-large-file-mode fundamental-mode "guard-lf")
  :config
  (cl-callf append guard-lf-intact-major-modes '(rosbag-info-mode ein:ipynb-mode)))


;; Utilities for opening files with "sudo"
(use-package sudo-edit
  :straight t
  :hook (minemacs-first-file . sudo-edit-indicator-mode))


;; Asynchronous "rsync" from `dired'
(use-package dired-rsync
  :straight (:files ("dired-rsync.el" "dired-rsync-transient.el"))
  :bind (:map
         dired-mode-map
         ("C-c C-r" . dired-rsync)
         ("C-c C-x" . dired-rsync-transient)))


;; Directory tree comparison mode for Emacs (inspired by commercial tools like Beyond Compare and Araxis Merge)
(use-package ztree
  :straight (:source gnu-elpa-mirror))


(provide 'me-files)

;;; me-files.el ends here
