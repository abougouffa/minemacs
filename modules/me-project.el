;;; me-project.el --- Projects stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


(use-package projectile
  :straight t
  :after minemacs-loaded
  :general
  (+map
    ;; Project
    :infix "p"
    "a"  '(projectile-add-known-project :wk "Add")
    "D"  '(projectile-edit-dir-locals :wk "Edit dir-locals")
    "<" #'projectile-switch-open-project
    ;; Compile/test
    "c"  '(nil :wk "compile/test")
    "cc" #'projectile-compile-project
    "cg" #'projectile-configure-project
    "ct" #'projectile-test-project
    "ci" #'projectile-install-project
    "cp" #'projectile-package-project
    "r"  '(nil :wk "run")
    "rr" #'projectile-run-project
    "rg" #'projectile-run-gdb
    "rt" #'projectile-run-vterm
    "re" #'projectile-run-eshell
    "rs" #'projectile-run-shell
    "rR" #'projectile-run-command-in-root
    "rS" #'projectile-run-shell-command-in-root
    "rA" #'projectile-run-async-shell-command-in-root
    ;; Forget
    "F"  '(nil :wk "forget")
    "Fz" '(projectile-cleanup-known-projects :wk "Zombie projects")
    "Fp" '(projectile-remove-known-project :wk "Project")
    "FP" '(projectile-remove-current-project-from-known-projects :wk "Current project")
    ;; Search/replace
    "s"  '(nil :wk "search/replace")
    "ss" 'projectile-grep
    "sn" '(fileloop-continue :wk "Next match")
    "sr" #'projectile-replace-regexp)
  :custom
  (projectile-cache-file (+directory-ensure (concat minemacs-cache-dir "projectile/cache.el")))
  (projectile-known-projects-file (concat minemacs-local-dir "projectile/known-projects.el"))
  (projectile-ignored-projects '("~/"))
  (projectile-ignored-project-function nil) ;; TODO: customize it
  (projectile-auto-discover nil)
  (projectile-enable-caching (not noninteractive))
  (projectile-globally-ignored-files '("TAGS" ".Trash" ".DS_Store"))
  (projectile-globally-ignored-file-suffixes '(".elc" ".eln" ".pyc" ".o"))
  (projectile-kill-buffers-filter 'kill-only-files)
  :init
  (global-set-key [remap evil-jump-to-tag] #'projectile-find-tag)
  (global-set-key [remap find-tag] #'projectile-find-tag)
  :config
  ;; HACK: from Doom Emacs
  ;; Projectile cleans up the known projects list at startup. If this list
  ;; contains tramp paths, the `file-remote-p' calls will pull in tramp via
  ;; its `file-name-handler-alist' entry, which is expensive. Since Doom
  ;; already cleans up the project list on kill-emacs-hook, it's simplest to
  ;; inhibit this cleanup process at startup (see bbatsov/projectile#1649).
  (cl-letf (((symbol-function 'projectile--cleanup-known-projects) #'ignore))
    (projectile-mode +1)))

(use-package consult-projectile
  :straight t
  :general
  (+map
    ":"  '(consult-projectile-find-file :wk "Find file in project")
    ;; Buffer
    "bp" #'consult-projectile-switch-to-buffer
    ;; Project
    "pp" #'consult-projectile
    "pP" '(consult-projectile-switch-project :wk "Switch")
    "pR" #'consult-projectile-recentf
    "pd" '(consult-projectile-find-dir :wk "Find directory")
    "pf" '(consult-projectile-find-file :wk "Find file")))

(use-package treemacs-projectile
  :straight t
  :after projectile)


(provide 'me-project)
