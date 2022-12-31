;;; me-vc.el --- Git and version control -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


(add-to-list 'auto-mode-alist '("\\.gitignore\\'" . conf-mode))

(use-package magit
  :straight t
  :general
  (+map
    :infix "g"
    "g" #'magit-status
    "C" #'magit-clone
    "b" #'magit-blame
    "l" #'magit-log
    "d" #'magit-diff-dwim
    "d" #'magit-stage)
  :custom
  (magit-diff-refine-hunk t)
  (magit-revision-show-gravatars t)
  (magit-save-repository-buffers nil)
  ;; Show in new window
  (magit-display-buffer-function #'magit-display-buffer-fullcolumn-most-v1)
  :config
  ;; Map ESC and q to quit transient
  (define-key transient-map [escape]  #'transient-quit-one)
  (define-key transient-map (kbd "q") #'transient-quit-one))


(use-package forge
  :straight t
  :after magit
  :init
  ;; Keybindings will be overriten by evil-collection
  (setq forge-add-default-bindings nil)
  :custom
  (forge-database-file (concat minemacs-local-dir "forge/database.sqlite")))

(use-package diff-hl
  :straight t
  :hook (find-file    . diff-hl-mode)
  :hook (dired-mode   . diff-hl-dired-mode)
  :hook (vc-dir-mode  . diff-hl-dir-mode)
  :hook (diff-hl-mode . diff-hl-flydiff-mode)
  :general
  (+map "gs" #'diff-hl-stage-current-hunk)
  :custom
  (diff-hl-draw-borders nil)
  :config
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

(use-package git-timemachine
  :straight t
  :general
  (+map
    "gt" '(git-timemachine-toggle :wk "Time machine"))
  :custom
  (git-timemachine-show-minibuffer-details t))

;; Enforce git commit conventions.
;; See https://chris.beams.io/posts/git-commit/
(use-package git-commit
  :after magit
  :custom
  (git-commit-summary-max-length 50)
  (git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line))
  :config
  (add-hook
   'git-commit-mode-hook
   (defun +git-gommit--set-fill-column-h ()
     (setq-local fill-column 72)))
  (add-hook
   'git-commit-setup-hook
   ;; Enter evil-insert-state for new commits
   (defun +git-commit--enter-evil-insert-state-maybe-h ()
     (when (and (bound-and-true-p evil-mode)
                (not (evil-emacs-state-p))
                (bobp)
                (eolp))
       (evil-insert-state))))
  (global-git-commit-mode 1))

(use-package smerge-mode
  :straight t
  :general
  (+map "gm" '(+smerge-hydra/body :wk "sMerge"))
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
  (defconst REPO-P (executable-find "repo"))
  :when REPO-P
  :general
  (+map "gr" #'repo-status))

(provide 'me-vc)
