;;; me-project.el --- Projects stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(use-package project
  :straight (:type built-in)
  :after minemacs-loaded
  :demand t
  :custom
  (project-list-file (concat minemacs-local-dir "project-list.el"))
  (project-vc-extra-root-markers '(".projectile.el" ".project.el" ".project"))
  :init
  (+map! ":"  #'project-find-file)
  (+map! :infix "p" ;; project
    "w"  #'project-switch-project
    "c"  #'project-compile
    "d"  #'project-find-dir
    "f"  #'project-find-file
    "k"  #'project-kill-buffers
    "b"  #'project-switch-to-buffer
    "a"  #'+project-add-project
    "D"  #'+dir-locals-open-or-create
    "-"  #'project-dired
    ;; compile/test
    "c" #'project-compile
    ;; run
    "r"  '(nil :wk "run")
    "re" #'project-eshell
    "rg" #'+project-gdb
    "rs" #'project-shell
    "rc" #'project-shell-command
    "rC" #'project-async-shell-command
    ;; forget
    "F"  '(nil :wk "forget/cleanup")
    "Fz" '(project-forget-zombie-projects :wk "Zombie projects")
    "Fp" '(project-forget-project :wk "Project")
    "Fu" '(project-forget-projects-under :wk "Projects under...")
    ;; search/replace
    "s"  '(nil :wk "search/replace")
    "ss" #'project-search
    "sn" '(fileloop-continue :wk "Next match")
    "sr" #'project-query-replace-regexp
    "sf" #'project-find-regexp))

(use-package consult-project-extra
  :straight t
  :init
  (+map! :infix "p" ;; project
    "p" #'consult-project-extra-find
    "P" #'consult-project-extra-find-other-window))

(use-package ibuffer-project
  :straight t
  :hook (ibuffer . +ibuffer-project-h)
  :config
  ;; From Crafted Emacs
  (defun +ibuffer-project-h ()
    (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
    (unless (eq ibuffer-sorting-mode 'project-file-relative)
      (ibuffer-do-sort-by-project-file-relative))))


(provide 'me-project)

;;; me-project.el ends here
