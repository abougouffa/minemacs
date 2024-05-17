;;; me-files.el --- File management -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package dirvish
  :straight t
  :custom
  (dirvish-attributes '(subtree-state nerd-icons file-size))
  (dirvish-cache-dir (+directory-ensure minemacs-cache-dir "dirvish/"))
  (dirvish-mode-line-format '(:left (sort file-time symlink) :right (omit yank index)))
  (dirvish-side-width 30)
  (dirvish-fd-default-dir "~/")
  (dirvish-use-header-line t) ; 'global make header line span all panes
  (dirvish-use-mode-line t)
  :init
  (+map!
    ;; Open
    "o-" #'dirvish
    "op" #'dirvish-side
    "oq" #'dirvish-quick-access
    ;; Search
    "sd" #'dirvish-fd)
  ;; Load immediately if Emacs is launched in an "open with" fashion
  (when minemacs-started-with-extra-args-p (require 'dirvish))
  :config
  (+nvmap! :keymaps 'dirvish-mode-map
    "q" #'dirvish-quit
    "s" #'dirvish-subtree-toggle
    "y" #'dirvish-yank-menu)

  ;; Cscope generate *.po files which that makes dirvish preview freeze
  (push "po" dirvish-preview-disabled-exts)

  (dirvish-override-dired-mode 1))

(use-package vlf-setup
  :straight vlf
  :after minemacs-loaded
  :demand t)

(use-package sudo-edit
  :straight t
  :hook (minemacs-first-file . sudo-edit-indicator-mode)
  :init
  (+map!
    "fF" #'sudo-edit-find-file
    "fu" #'sudo-edit))

(use-package dired-rsync
  :straight (:files ("dired-rsync.el" "dired-rsync-transient.el"))
  :bind (:map
         dired-mode-map
         ("C-c C-r" . dired-rsync)
         ("C-c C-x" . dired-rsync-transient)))

(use-package ztree
  :straight t
  :init
  (+map! "oz" #'ztree-diff))


(provide 'me-files)

;;; me-files.el ends here
