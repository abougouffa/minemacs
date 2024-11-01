;;; me-vc.el --- Git and version control -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;; It's Magit! A Git Porcelain inside Emacs.
(use-package magit
  :straight t
  :custom
  (magit-diff-refine-hunk t)
  (magit-revision-show-gravatars t)
  (magit-save-repository-buffers nil)
  (magit-display-buffer-function #'magit-display-buffer-fullcolumn-most-v1) ; Show in new window
  :init
  ;; Replace the `project-vc-dir' by `magit-project-status' in project prefix and switch commands
  (with-eval-after-load 'project
    (keymap-set project-prefix-map "v" 'magit-project-status)
    (when-let ((vc (assoc 'project-vc-dir project-switch-commands)))
      (setcar vc 'magit-project-status)
      (setcdr vc '("Magit project status"))))
  :config
  ;; Automatically refresh Magit after save
  (add-hook 'after-save-hook 'magit-after-save-refresh-status))


;; Edit Git commit messages - part of `magit'
(use-package git-commit
  :after magit
  :commands (global-git-commit-mode)
  :custom
  (git-commit-summary-max-length 72) ; defaults to Github's max commit message length
  (git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line))
  :init
  (global-git-commit-mode 1))


;; Show source files' TODOs (and FIXMEs, etc) in Magit status buffer
(use-package magit-todos
  :straight t)


;; File icons for Magit based on `nerd-icons'
(use-package magit-file-icons
  :straight t
  :trigger-commands magit magit-status magit-status-here magit-log magit-log-all
  :init
  (magit-file-icons-mode 1))


;; Magit extension for "git-imerge"
(use-package magit-imerge
  :straight t
  :after magit
  :init
  (transient-append-suffix 'magit-merge "m" '("M" "magit-imerge" magit-imerge)))


;; Store EIEIO objects using EmacSQL
(use-package closql
  :straight t)


;; Work with Git forges from the comfort of Magit
(use-package forge
  :straight t
  :after magit
  :demand
  :config
  (require 'on-demand/me-markdown))


;; Emacs package for highlighting uncommitted changes
(use-package diff-hl
  :straight t
  :hook (find-file . diff-hl-mode)
  :hook (dired-mode . diff-hl-dired-mode)
  :hook (vc-dir-mode . diff-hl-dir-mode)
  :hook (diff-hl-mode . diff-hl-flydiff-mode)
  :hook (magit-pre-refresh . diff-hl-magit-pre-refresh)
  :hook (magit-post-refresh . diff-hl-magit-post-refresh))


;; Walk through git revisions of a file
(use-package git-timemachine
  :straight t
  :hook (git-timemachine-mode . +git-timemachine--run-delayed-mode-hooks-h)
  :config
  ;; BUG+FIX: `git-timemachine' applies the mode with `delay-mode-hooks'. For
  ;; some reason, this makes the buffer losing it's font lock. Hence, we
  ;; explicitly run the delayed hooks after applying the `git-timemachine-mode'
  ;; (see the implementation of `git-timemachine--start').
  (defun +git-timemachine--run-delayed-mode-hooks-h ()
    (run-mode-hooks 'delayed-mode-hooks)))


;; Emacs major modes for Git configuration files
(use-package git-modes
  :straight t
  :mode ("/.dockerignore\\'" . gitignore-mode))


;; Running "repo" from Emacs
(use-package repo
  :straight t)


;; Transient menus to use some "repo" commands within Magit
(use-package repo-transient
  :straight (:type git :repo "https://chromium.googlesource.com/chromiumos/platform/dev-util" :files ("contrib/emacs/gerrit/repo-transient.el"))
  :commands (repo-main-menu))


;; View diffs side-by-side in Emacs
(use-package diffview
  :straight t)


(provide 'me-vc)

;;; me-vc.el ends here
