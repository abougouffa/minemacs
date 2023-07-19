;;; me-vc.el --- Git and version control -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

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
  :after magit
  :demand t
  :config
  (magit-todos-mode 1))

(use-package magit-imerge
  :straight t
  :init
  (with-eval-after-load 'magit
    (transient-append-suffix 'magit-merge "m"
      '("M" "magit-imerge" magit-imerge))))

(use-package closql
  :straight t)

(use-package forge
  :straight t
  :after magit
  :demand t
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

(use-package emojify ;; Needed by `code-review'
  :straight t
  :custom
  (emojify-emoji-set "emojione-v2.2.6")
  (emojify-emojis-dir (concat minemacs-cache-dir "emojify/emojis/"))
  (emojify-display-style 'image)
  (emojify-download-emojis-p t)
  :init
  (when (< emacs-major-version 29)
    (+map! "ie" '(emojify-insert-emoji :wk "Emoji"))))

(use-package code-review
  :straight (:host github :repo "phelrine/code-review" :branch "fix/closql-update")
  :after magit
  :custom
  (code-review-download-dir (concat minemacs-cache-dir "code-review/"))
  (code-review-db-database-file (concat minemacs-local-dir "code-review/database.sqlite"))
  (code-review-log-file (concat minemacs-local-dir "code-review/code-review-error.log"))
  (code-review-auth-login-marker 'forge) ; use the same credentials as forge in ~/.authinfo.gpg
  :init
  (with-eval-after-load 'magit
    (transient-append-suffix 'magit-merge "i"
      '("y" "Review pull-request" code-review-forge-pr-at-point)))
  (with-eval-after-load 'forge
    (transient-append-suffix 'forge-dispatch "c u"
      '("c r" "review pull-request" code-review-forge-pr-at-point))))

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

;; Enforce git commit conventions.
;; See: chris.beams.io/posts/git-commit/
(use-package git-commit
  :after magit
  :demand t
  :custom
  (git-commit-summary-max-length 72) ;; defaults to Github's max commit message length
  (git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line))
  :config
  (evil-set-initial-state 'git-commit-mode 'insert)
  (global-git-commit-mode 1))

(use-package git-modes
  :straight t
  :init
  :mode ("/.dockerignore\\'" . gitignore-mode))

(use-package smerge-mode
  :straight t
  :init
  (+map! "gm" '(+smerge-hydra/body :wk "sMerge"))
  :config
  (defhydra +smerge-hydra (:hint nil
                                 :pre (if (not smerge-mode) (smerge-mode 1))
                                 ;; Disable `smerge-mode' when quitting hydra if
                                 ;; no merge conflicts remain.
                                 :post (smerge-auto-leave))
    "
                                                         [smerge]
  Movement   Keep           Diff              Other         │
  ╭─────────────────────────────────────────────────────────╯
  │  ^_g_^       [_b_] base       [_<_] upper/base    [_C_] Combine
  │  ^_C-k_^     [_u_] upper      [_=_] upper/lower   [_r_] resolve
  │  ^_k_ ↑^     [_l_] lower      [_>_] base/lower    [_R_] remove
  │  ^_j_ ↓^     [_a_] all        [_H_] hightlight    [_n_] next in project
  │  ^_C-j_^     [_RET_] current  [_E_] ediff
  │  ^_G_^                                                 [_q_] quit
  ╰─────────────────────────────────────────────────────╯
"
    ("g" (progn (goto-char (point-min)) (smerge-next)))
    ("G" (progn (goto-char (point-max)) (smerge-prev)))
    ("C-j" smerge-next)
    ("C-k" smerge-prev)
    ("j" next-line)
    ("k" previous-line)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("H" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("R" smerge-kill-current)
    ;; Often after calling `smerge-vc-next-conflict', the cursor will land at
    ;; the bottom of the window
    ("n" (progn (smerge-vc-next-conflict) (recenter-top-bottom (/ (window-height) 8))))
    ("q" nil :color blue)))

(use-package repo
  :straight t
  :preface
  (defconst +repo-available-p (executable-find "repo"))
  :when +repo-available-p
  :init
  (+map! "gr" #'repo-status))


(provide 'me-vc)

;;; me-vc.el ends here
