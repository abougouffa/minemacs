;; -*- lexical-binding: t; -*-


(use-package projectile
  :straight t
  :after minemacs-loaded
  :general
  (+map
    ":"   #'projectile-find-file
    ;; Buffer
    "bp"  #'projectile-switch-to-buffer
    ;; Project
    "pa"  '(projectile-add-known-project :wk "Add")
    "pD"  '(projectile-edit-dir-locals :wk "Edit dir-locals")
    ;;
    "pc"  '(nil :wk "compile/test")
    "pcc"  #'projectile-compile-project
    "pcg"  #'projectile-configure-project
    "pct"  #'projectile-test-project
    "pci"  #'projectile-install-project
    ;; Forget
    "pF"  '(nil :wk "Forget")
    "pFz" '(projectile-cleanup-known-projects :wk "Zombie projects")
    "pFp" '(projectile-remove-known-project :wk "Project")
    "pFP" '(projectile-remove-current-project-from-known-projects :wk "Current project")
    ;; Search/replace
    "ps"  '(nil :wk "Search/replace")
    "pss" 'projectile-grep
    "psn" '(fileloop-continue :wk "Next match")
    "psr" #'projectile-replace-regexp)
  :custom
  (projectile-cache-file (concat minemacs-cache-dir "projectile.cache"))
  (projectile-known-projects-file (concat minemacs-cache-dir "projectile.projects"))
  (projectile-ignored-projects '("~/"))
  (projectile-ignored-project-function nil) ;; TODO: customize it
  (projectile-auto-discover nil)
  (projectile-enable-caching (not noninteractive))
  (projectile-globally-ignored-files '("TAGS" ".Trash" ".DS_Store"))
  (projectile-globally-ignored-file-suffixes '(".elc" ".eln" ".pyc" ".o"))
  (projectile-kill-buffers-filter 'kill-only-files)
  :init
  (global-set-key [remap evil-jump-to-tag] #'projectile-find-tag)
  (global-set-key [remap find-tag]         #'projectile-find-tag)
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
    "pp"  #'consult-projectile
    "pP"  '(consult-projectile-switch-project :wk "Switch")
    "pr"  #'consult-projectile-recentf
    "pd"  '(consult-projectile-find-dir :wk "Find directory")
    "pf"  '(consult-projectile-find-file :wk "Find file")))


;; Use keybindings for built-in project.el
(unless t
  (+map
    ":"   #'project-find-file
    ;; Buffer
    "bp"  #'project-switch-to-buffer
    ;; Project
    "pp"  '(project-switch-project :wk "Switch")
    "pc"  '(project-compile :wk "Compile")
    "pd"  '(project-find-dir :wk "Find directory")
    "pf"  '(project-find-file :wk "Find file")
    "pD"  '(+dir-locals-open-or-create :wk "Open/create dir-locals file")
    ;; Forget
    "pF"  '(nil :wk "Forget")
    "pFz" '(project-forget-zombie-projects :wk "Zombie projects")
    "pFp" '(project-forget-project :wk "Project")
    "pFu" '(project-forget-projects-under :wk "Projects under...")
    ;; Search/replace
    "ps"  '(nil :wk "Search/replace")
    "pss" '(project-search :wk "Search")
    "psn" '(fileloop-continue :wk "Next match")
    "psr" #'project-query-replace-regexp
    "psf" #'project-find-regexp))


(provide 'me-project)
