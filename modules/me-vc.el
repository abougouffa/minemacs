;;; me-vc.el --- Git and version control -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package magit
  :straight t
  :init
  (+map! :infix "g"
    "g" #'magit-status
    "C" #'magit-clone
    "b" #'magit-blame
    "l" #'magit-log
    "d" #'magit-diff-dwim
    "s" #'magit-stage
    "i" #'magit-init)
  :custom
  (magit-diff-refine-hunk t)
  (magit-revision-show-gravatars t)
  (magit-save-repository-buffers nil)
  ;; Show in new window
  (magit-display-buffer-function #'magit-display-buffer-fullcolumn-most-v1))

(use-package magit-todos
  :straight t
  :init
  (+map-local! :keymaps 'magit-status-mode-map
    "t" `(,(+cmdfy! (magit-todos-mode 'toggle) (magit-refresh)) :wk "magit-todos-mode")))

(use-package magit-file-icons
  :straight t
  :after magit-status
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
  :demand
  :preface
  ;; Keybindings will be overriten by `evil-collection'
  (setq forge-add-default-bindings nil)
  :init
  (+map! :infix "g"
    "f" '(nil :wk "forge")
    "ff" #'forge-dispatch
    "fc" #'forge-create-post
    "fe" #'forge-edit-post
    "ft" #'forge-edit-topic-title
    "fs" #'forge-edit-topic-state
    "fd" #'forge-edit-topic-draft)
  :custom
  (forge-database-file (concat minemacs-local-dir "forge/database.sqlite"))
  :config
  (transient-append-suffix 'forge-dispatch "M"
    '("m" "forge merge (via API)" forge-merge)))

(use-package diff-hl
  :straight t
  :hook (find-file . diff-hl-mode)
  :hook (dired-mode . diff-hl-dired-mode)
  :hook (vc-dir-mode . diff-hl-dir-mode)
  :hook (diff-hl-mode . diff-hl-flydiff-mode)
  :hook (magit-pre-refresh . diff-hl-magit-pre-refresh)
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :init
  (+map! "gs" #'diff-hl-stage-current-hunk)
  :custom
  (diff-hl-draw-borders nil))

(use-package git-timemachine
  :straight t
  :init
  (+map! "gt" #'git-timemachine-toggle)
  :custom
  (git-timemachine-show-minibuffer-details t))

;; Enforce git commit conventions. See: chris.beams.io/posts/git-commit
(use-package git-commit
  :after magit
  :demand
  :custom
  (git-commit-summary-max-length 72) ; defaults to Github's max commit message length
  (git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line))
  :config
  (with-eval-after-load 'evil (evil-set-initial-state 'git-commit-mode 'insert))
  (global-git-commit-mode 1))

(use-package git-modes
  :straight t
  :init
  :mode ("/.dockerignore\\'" . gitignore-mode))

(use-package repo
  :straight t
  :init
  (+map!
    "gr" '(nil :wk "repo")
    "grg" #'repo-status))

(use-package gee
  :straight (:host bitbucket :repo "olanilsson/gee"))

;; https://chromium.googlesource.com/chromiumos/platform/dev-util/+/HEAD/contrib/emacs/gerrit/README.md
(use-package gerrit
  :straight (chromeos-gerrit :type git :repo "https://chromium.googlesource.com/chromiumos/platform/dev-util" :files ("contrib/emacs/gerrit/*")))

(use-package repo-transient
  :straight chromeos-gerrit
  :commands repo-sync repo-prune repo-start repo-start-temp repo-rebase repo-upload-all repo-upload-current
  repo-upload-menu repo-upload-menu-with-repohooks repo-start-menu repo-sync-menu repo-rebase-menu repo-main-menu
  repo:all-projects repo:current-project
  :init
  (+map! "grr" #'repo-main-menu))

(use-package diffview
  :straight t
  :init
  (with-eval-after-load 'diff-mode
    (+map-local! :keymaps 'diff-mode-map
      "v" #'diffview-current
      "V" #'diffview-region))
  :config
  (+nvmap! :keymaps 'diffview--mode-map
    "="   #'diffview--align-windows
    "+"   #'diffview--align-windows
    "C-j" #'diffview--next-file
    "C-k" #'diffview--prev-file
    "q"   #'diffview--quit))


(provide 'me-vc)

;;; me-vc.el ends here
