;;; me-project.el --- Projects stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


(use-package projectile
  :straight t
  :after minemacs-loaded
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
  (+map
    ;; Project
    :infix "p"
    "a"  '(projectile-add-known-project :wk "Add")
    "D"  '(projectile-edit-dir-locals :wk "Edit dir-locals")
    ;; Compile/test
    "c"  '(nil :wk "compile/test")
    "cc"  #'projectile-compile-project
    "cg"  #'projectile-configure-project
    "ct"  #'projectile-test-project
    "ci"  #'projectile-install-project
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
    "bp" #'consult-projectile-switch-to-buffer)
  (+map
    :infix "p"
    ;; Project
    "p" #'consult-projectile
    "P" '(consult-projectile-switch-project :wk "Switch")
    "r" #'consult-projectile-recentf
    "d" '(consult-projectile-find-dir :wk "Find directory")
    "f" '(consult-projectile-find-file :wk "Find file")))


(provide 'me-project)
