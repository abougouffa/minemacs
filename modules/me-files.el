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
    "oq" #'dirvish-quick-access
    ;; Search
    "sd" #'dirvish-fd)
  ;; Load immediately if Emacs is launched in an "open with" fashion
  (when minemacs-started-with-extra-args-p (require 'dirvish))
  :config
  ;; Cscope generate *.po files which that makes dirvish preview freeze
  (push "po" dirvish-preview-disabled-exts)
  (dirvish-override-dired-mode 1))

(use-package neotree
  :straight (:host github :repo "abougouffa/emacs-neotree" :branch "feat/nerd-icons")
  :custom
  (neo-theme 'nerd-icons)
  :init
  (+map! "op" #'neotree-toggle))

(use-package vlf-setup
  :straight (vlf :source gnu-elpa-mirror)
  :after minemacs-loaded
  :config
  (add-to-list 'so-long-mode-preserved-variables 'vlf-mode))

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
  :straight (:source gnu-elpa-mirror)
  :init
  (+map! "oz" #'ztree-diff))


(provide 'me-files)

;;; me-files.el ends here
