;;; me-vc.el --- Git and version control -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

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

(use-package magit-todos
  :straight t)

;; BUG+TEMP: It seems like `nerd-file-icons' is causing issues with the latest
;; `magit' update
(push 'magit-file-icons minemacs-disabled-packages)
(use-package magit-file-icons
  :straight t
  :after magit
  :init
  (magit-file-icons-mode 1))

(use-package magit-imerge
  :straight t
  :after magit
  :init
  (transient-append-suffix 'magit-merge "m" '("M" "magit-imerge" magit-imerge)))

(use-package closql
  :straight t)

(use-package forge
  :straight t
  :after magit
  :demand)

(use-package diff-hl
  :straight t
  :hook (find-file . diff-hl-mode)
  :hook (dired-mode . diff-hl-dired-mode)
  :hook (vc-dir-mode . diff-hl-dir-mode)
  :hook (diff-hl-mode . diff-hl-flydiff-mode)
  :hook (magit-pre-refresh . diff-hl-magit-pre-refresh)
  :hook (magit-post-refresh . diff-hl-magit-post-refresh))

(use-package git-timemachine
  :straight t
  :custom
  (git-timemachine-show-minibuffer-details t))

(use-package git-commit
  :after magit
  :commands (global-git-commit-mode)
  :custom
  (git-commit-summary-max-length 72) ; defaults to Github's max commit message length
  (git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line))
  :init
  (global-git-commit-mode 1))

(use-package git-modes
  :straight t
  :mode ("/.dockerignore\\'" . gitignore-mode))

(use-package repo
  :straight t)

;; https://chromium.googlesource.com/chromiumos/platform/dev-util/+/HEAD/contrib/emacs/gerrit/README.md
(use-package repo-transient
  :straight (:type git :repo "https://chromium.googlesource.com/chromiumos/platform/dev-util" :files ("contrib/emacs/gerrit/repo-transient.el"))
  :commands (repo-main-menu))

(use-package diffview
  :straight t)


(provide 'me-vc)

;;; me-vc.el ends here
