;; -*- lexical-binding: t; -*-

(use-package magit
  :straight t
  :general
  (me-map
    "gg" '(magit-status :which-key "Status")
    "gC" '(magit-clone :which-key "Clone repo"))
  :custom
  (magit-diff-refine-hunk t)
  (magit-revision-show-gravatars t)
  (magit-save-repository-buffers nil)
  ;; Show in new window
  (magit-display-buffer-function 'magit-display-buffer-fullcolumn-most-v1))


(use-package forge
  :straight t
  :after magit
  :init
  ;; Keybindings will be overriten by evil-collection
  (setq forge-add-default-bindings nil)
  :custom
  (forge-database-file (expand-file-name "forge/database.sqlite" minemacs-local-dir)))


(use-package diff-hl
  :straight t
  :hook (find-file    . diff-hl-mode)
  :hook (dired-mode   . diff-hl-dired-mode)
  :hook (vc-dir-mode  . diff-hl-dir-mode)
  :hook (diff-hl-mode . diff-hl-flydiff-mode)
  :hook (diff-hl-mode . diff-hl-show-hunk-mouse-mode)
  :general
  (me-map
    "gs" '(diff-hl-stage-current-hunk :which-key "Stage hunk at point"))
  :custom
  (diff-hl-draw-borders t)
  :config
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))


(use-package git-timemachine
  :straight t
  :after minemacs-loaded
  :general
  (me-map
    "gt" '(git-timemachine-toggle :which-key "Time machine"))
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
  (add-hook 'git-commit-mode-hook (lambda () (setq-local fill-column 72)))
  (add-hook
   'git-commit-setup-hook
   ;; Enter evil-insert-state for new commits
   (lambda ()
     (when (and (bound-and-true-p evil-mode)
                (not (evil-emacs-state-p))
                (bobp)
                (eolp))
       (evil-insert-state))))
  (global-git-commit-mode 1))


(use-package smerge-mode
  :straight t
  :general
  (me-map
    "gm" '(+smerge-hydra/body :which-key "sMerge"))
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


(provide 'me-vc)
