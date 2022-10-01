;; -*- lexical-binding: t; -*-

(use-package magit
  :straight t
  :commands (magit magit-status))

(use-package diff-hl
  :straight t
  :commands (diff-hl-mode diff-hl-stage-current-hunk))

(use-package git-timemachine
  :straight t
  :commands (git-timemachine git-timemachine-toggle)
  :config
  (setq git-timemachine-show-minibuffer-details t))

;; (use-package! git-commit
;;   :hook (doom-first-file . global-git-commit-mode)
;;   :config
;;   (set-yas-minor-mode! 'git-commit-mode)

;;   ;; Enforce git commit conventions.
;;   ;; See https://chris.beams.io/posts/git-commit/
;;   (setq git-commit-summary-max-length 50
;;         git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line))
;;   (setq-hook! 'git-commit-mode-hook fill-column 72)

;;   (add-hook! 'git-commit-setup-hook
;;     (defun +vc-start-in-insert-state-maybe-h ()
;;       "Start git-commit-mode in insert state if in a blank commit message,
;; otherwise in default state."
;;       (when (and (bound-and-true-p evil-mode)
;;                  (not (evil-emacs-state-p))
;;                  (bobp) (eolp))
;;         (evil-insert-state)))))

;;;###autoload
(defhydra +vc/smerge-hydra (:hint nil
                            :pre (if (not smerge-mode) (smerge-mode 1))
                            ;; Disable `smerge-mode' when quitting hydra if
                            ;; no merge conflicts remain.
                            :post (smerge-auto-leave))
    "
                                                         [smerge]
  Movement   Keep           Diff              Other
  ╭─────────────────────────────────────────────────────────╯
     ^_g_^       [_b_] base       [_<_] upper/base    [_C_] Combine
     ^_C-k_^     [_u_] upper      [_=_] upper/lower   [_r_] resolve
     ^_k_ ↑^     [_l_] lower      [_>_] base/lower    [_R_] remove
     ^_j_ ↓^     [_a_] all        [_H_] hightlight    [_n_] next in project
     ^_C-j_^     [_RET_] current  [_E_] ediff                 ╭──────────
     ^_G_^                                                │ [_q_] quit
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
    ("q" nil :color blue))
