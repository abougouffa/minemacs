;;; me-files.el --- File management -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(defconst minemacs-started-with-directory-p
  (let ((args (cdr command-line-args))) (cl-some #'file-directory-p args)))

(use-package dirvish
  :straight t
  :demand minemacs-started-with-directory-p
  :custom
  (dirvish-attributes '(subtree-state nerd-icons file-size vc-state git-msg))
  (dirvish-cache-dir (+directory-ensure minemacs-cache-dir "dirvish/"))
  (dirvish-mode-line-format '(:left (sort file-time symlink) :right (omit yank index)))
  (dirvish-side-width 30)
  (dirvish-fd-default-dir "~/")
  (dirvish-use-header-line t) ; 'global make header line span all panes
  (dirvish-use-mode-line t)
  :init
  (+map!
    ;; Open
    "o-" '(dirvish :wk "Dirvish")
    "oq" '(dirvish-quick-access :wk "Dirvish quick access")
    ;; Search
    "sd" '(dirvish-fd :wk "Dirvish fd"))
  :config
  (+nvmap! :keymaps 'dirvish-mode-map
    "q" #'dirvish-quit
    "s" #'dirvish-subtree-toggle
    "y" #'dirvish-yank-menu)

  (dirvish-override-dired-mode 1))

(use-package vlf-setup
  :straight vlf
  :after minemacs-loaded
  :demand t)

(use-package treemacs
  :straight t
  :init
  (+map! "op" #'treemacs)
  :custom
  (treemacs-persist-file (concat minemacs-local-dir "treemacs/persist.el"))
  (treemacs-last-error-persist-file (concat minemacs-local-dir "treemacs/last-error-persist.el"))
  (treemacs-width 30))

(use-package treemacs-nerd-icons
  :straight t
  :after treemacs nerd-icons
  :demand t
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package sudo-edit
  :straight t
  :hook (minemacs-first-file . sudo-edit-indicator-mode)
  :init
  (+map!
    "fF" #'sudo-edit-find-file
    "fu" #'sudo-edit))

(use-package dired-rsync
  :straight (dired-rsync :files ("dired-rsync.el" "dired-rsync-transient.el"))
  :when (executable-find "rsync")
  :bind (:map
         dired-mode-map
         ("C-c C-r" . dired-rsync)
         ("C-c C-x" . dired-rsync-transient)))

(use-package ztree
  :straight t
  :init
  (+map! "oz" #'ztree-diff))

(defconst +sr-speedbar-path
  (+package-download-from-urls 'sr-speedbar "https://www.emacswiki.org/emacs/download/sr-speedbar.el"))

(use-package sr-speedbar
  :load-path +sr-speedbar-path
  :commands sr-speedbar-toggle sr-speedbar-open
  :custom
  (sr-speedbar-right-side nil)
  :config
  (setq speedbar-use-images ezimage-use-images)
  (+nvmap! :keymaps 'speedbar-mode-map "q" #'sr-speedbar-close))


(provide 'me-files)

;;; me-files.el ends here
