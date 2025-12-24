;;; me-vc.el --- Git and version control -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2022-10-02
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

(use-package cond-let :ensure t) ; dependency of `magit'

;; It's Magit! A Git Porcelain inside Emacs.
(use-package magit
  :ensure t
  :custom
  (magit-diff-refine-hunk t)
  (magit-save-repository-buffers nil)
  (magit-format-file-function #'magit-format-file-nerd-icons)
  (magit-display-buffer-function #'magit-display-buffer-fullcolumn-most-v1) ; Show in new window
  :init
  ;; Replace the `project-vc-dir' by `magit-project-status' in project prefix and switch commands
  (with-eval-after-load 'project
    (keymap-set project-prefix-map "v" 'magit-project-status)
    (when-let* ((vc (assoc 'project-vc-dir project-switch-commands)))
      (setcar vc 'magit-project-status)
      (setcdr vc '("Magit project status"))))
  :config
  ;; Make `magit' collaborate with `tramp-direct-async-process' (magit/magit#5220)
  (setq magit-tramp-pipe-stty-settings 'pty))


;; Edit Git commit messages - part of `magit'
(use-package git-commit
  :after magit
  :commands (global-git-commit-mode)
  :hook (git-commit-setup . +git-insert-commit-prefix)
  :custom
  (git-commit-summary-max-length 72) ; defaults to Github's max commit message length
  (git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line))
  :init
  (global-git-commit-mode 1))


;; Magit extension for "git-imerge"
(use-package magit-imerge
  :ensure t
  :when (executable-find "git-imerge")
  :after magit
  :init
  (with-eval-after-load 'transient
    (transient-append-suffix 'magit-merge "m" '("M" "magit-imerge" magit-imerge))))


;; Use delta when viewing diffs in `magit'
(use-package magit-delta
  :vc (:url "https://github.com/dandavison/magit-delta")
  :when (executable-find "delta")
  :hook (magit-mode . magit-delta-mode)
  :custom
  (magit-delta-hide-plus-minus-markers nil))


;; Gerrit integration with Magit
(use-package magit-gerrit
  :vc (:url "https://github.com/abougouffa/magit-gerrit")
  :after magit
  :demand
  :bind ( :map magit-status-mode-map
          ("_" . magit-gerrit-mode)
          ("M-_" . magit-gerrit-dispatch)))


;; Gerrit integration via the REST API
(use-package gerrit
  :vc (:url "https://github.com/twmr/gerrit.el"))


;; Store EIEIO objects using EmacSQL
(use-package closql
  :ensure t)


;; Work with Git forges from the comfort of Magit
(use-package forge
  :ensure t
  :config
  (require 'on-demand/me-markdown))


;; Emacs package for highlighting uncommitted changes
(use-package diff-hl
  :ensure t
  :hook
  (minemacs-first-file . global-diff-hl-mode)
  (diff-hl-mode . +diff-hl-update-on-buffer-change)
  (dired-mode . diff-hl-dired-mode)
  :custom
  (diff-hl-draw-borders nil) ; Don't draw ugly borders
  :init
  ;; BUGFIX: Don't pollute the shared memory devfs (`diff-hl' uses "/dev/shm/" as
  ;; temporary directory!)
  (setq diff-hl-temporary-directory temporary-file-directory)
  :config
  ;; BUG+HACK: After commiting changes from `magit' and switching back to the
  ;; buffer, the diff-hl doesn't go away until an input happens. This hook will
  ;; ensure updating the `diff-hl' each time we switch to the buffer.
  (defun +diff-hl-update-on-buffer-change ()
    (add-hook 'window-buffer-change-functions (lambda (_win) (when diff-hl-mode (diff-hl-update))) nil t))
  (diff-hl-flydiff-mode 1))


;; Walk through Git revisions of a file
(use-package git-timemachine
  :ensure t
  :hook (git-timemachine-mode . display-line-numbers-mode)
  :config
  (advice-add
   'git-timemachine--show-minibuffer-details :around
   (satch-defun +git-timemachine--show-revision-in-header-line:around-a (orig-fn revision)
     "Show the current revision in the header-line instead of the echo area."
     (let ((inhibit-message t))
       (setq header-line-format
             (concat (format "  %s " (or (+nerd-icons-icon "nf-oct-git_branch") ""))
                     (funcall orig-fn revision)))))))


;; Emacs major modes for Git configuration files
(use-package git-modes
  :ensure t
  :mode ("/\\.\\(docker\\|fd\\|rg\\|ag\\|hg\\)?ignore\\'" . gitignore-mode))


;; Running "repo" from Emacs
(use-package repo
  :ensure t
  :hook (repo-mode . +ansi-color-apply-on-buffer))


;; Jujutsu (jj) integration with Emacs `vc' and `project'
(use-package vc-jj
  :ensure t
  :when (executable-find "jj")
  :after vc
  :demand)


;; View diffs side-by-side in Emacs
(use-package diffview
  :ensure t)


;; A structural diff that understands syntax
(use-package difftastic
  :ensure t
  :when (executable-find "difft"))


(provide 'me-vc)

;;; me-vc.el ends here
