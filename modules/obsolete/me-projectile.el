;;; me-projectile.el --- Projectile -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package projectile
  :straight t
  :after minemacs-loaded
  :demand
  :custom
  (projectile-cache-file (+directory-ensure minemacs-cache-dir "projectile/cache.el"))
  (projectile-known-projects-file (+directory-ensure minemacs-local-dir "projectile/known-projects.el"))
  (projectile-ignored-projects '("~/"))
  (projectile-auto-discover nil)
  (projectile-enable-caching (not noninteractive))
  (projectile-globally-ignored-files '("TAGS" ".Trash" ".DS_Store"))
  (projectile-globally-ignored-file-suffixes '(".elc" ".eln" ".pyc" ".o"))
  (projectile-kill-buffers-filter 'kill-only-files)
  :init
  (keymap-global-set "<remap> <find-tag>" 'projectile-find-tag)
  :config
  ;; HACK: Taken from Doom Emacs
  ;; 1. Projectile uses `file-remote-p' to check for remote (tramp) paths in its
  ;;    known project list, when it automatically cleans it up on
  ;;    `projectile-mode's activation. This causes `tramp' to be loaded, which
  ;;    is quite expensive.
  ;; 2. `file-remote-p' relies on an entry in `file-name-handler-alist'
  ;;    (autoloaded by `tramp') to detect remote paths, which causes `tramp' to
  ;;    be loaded. However, we set `file-name-handler-alist' to nil at startup
  ;;    for a noteable boost in startup performance. Normally, this is not an
  ;;    issue, as I defer `projectile-mode' until well after
  ;;    `file-name-handler-alist' is restored, but it is trivial for a user to
  ;;    inadvertantly load it too early (often as part of another package that
  ;;    depends on it, or by blindly following projectile's install instructions
  ;;    and calling `projectile-mode' themselves).

  ;; In order to address both of these, I defer projectile's cleanup process
  ;; altogether. Another approach I considered was to ensure `projectile-mode'
  ;; wasn't activated until the right time, regardless of when projectile is
  ;; loaded, but this may trouble savvier Emacs users who need projectile's API
  ;; early during startup, so it needs more consideration.
  (cl-letf (((symbol-function 'projectile--cleanup-known-projects) #'ignore))
    (projectile-mode 1))

  (add-hook 'kill-emacs-hook #'projectile--cleanup-known-projects))

(use-package consult-projectile
  :straight t)

(use-package treemacs-projectile
  :straight t
  :after projectile treemacs
  :demand)


(provide 'obsolete/me-projectile)
;;; me-projectile.el ends here
