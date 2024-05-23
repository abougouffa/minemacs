;;; me-tags.el --- Non-LSP source code tagging tools -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:


(use-package ggtags
  :straight t)

(use-package citre
  :straight t
  :after minemacs-first-c/c++-file
  :demand
  :custom
  ;; Better (!) project root detection function
  (citre-project-root-function #'+citre-recursive-project-root)
  :init
  (defcustom +citre-recursive-root-project-detection-files '(".tags/" ".repo/" ".citre_root")
    "A list of files/directories to use as a project root markers."
    :type '(repeat string)
    :group 'minemacs-prog)

  (defcustom +citre-gtags-recursive-files-list t
    "Find files to index recursively."
    :type 'boolean
    :group 'minemacs-prog)

  (defcustom +citre-gtags-files-list-suffixes '("*.[chly]" "*.[ch]xx" "*.[ch]pp" "*.[ch]++" "*.cc" "*.hh")
    "List of filename suffixes globs to index (for extensions for example)."
    :type '(repeat string)
    :group 'minemacs-prog)

  (defcustom +citre-gtags-files-list-ignored-directories '("CVS" "RCS" "SCCS" ".git" ".hg" ".bzr" ".cdv" ".pc" ".svn" ".repo" "_MTN" "_darcs" "_sgbak" "debian")
    "List of directories to be ignored when creating the file list using `+citre-gtags-find-files-command'."
    :type '(repeat string)
    :group 'minemacs-prog)
  :config
  (defun +citre-recursive-project-root ()
    "Search recursively until we find one of `+citre-recursive-root-project-detection-files'.
Fall back to the default `citre--project-root'."
    (or (+directory-root-containing-file +citre-recursive-root-project-detection-files)
        (citre--project-root))) ; Fall back to the default detection!

  (defun +citre-gtags-find-files-command (&optional dir)
    (let* ((default-directory (or dir default-directory)))
      (concat
       "echo 'Creating list of files to index ...'\n"
       (find-cmd
        (unless +citre-gtags-recursive-files-list '(maxdepth "1"))
        `(prune (and (type "d") (name ,@+citre-gtags-files-list-ignored-directories)))
        `(iname ,@+citre-gtags-files-list-suffixes)
        '(type "f" "l")
        '(print))
       " > gtags.files\n"
       "echo 'Creating list of files to index ... done'\n")))

  (defun +citre-gtags-create-list-of-files-to-index (top-directory)
    "Create a list of files to index in TOP-DIRECTORY."
    (interactive "DCreate file list in directory: ")
    (let* ((default-directory top-directory))
      (start-process-shell-command "+citre-gtags-files-list" "*+citre-gtags-files-list*" (+citre-gtags-find-files-command)))))

(use-package citre-config
  :straight citre
  :after citre
  :demand)

(use-package xcscope
  :straight t
  :unless os/win
  :commands cscope-create-list-of-files-to-index cscope-index-files)

(use-package clink
  :straight (:host github :repo "abougouffa/clink.el")
  :when (+emacs-features-p 'sqlite3)
  :hook (minemacs-first-c/c++-file . global-clink-mode))

(use-package rtags
  :straight t)

(use-package rtags-xref
  :straight t
  :commands rtags-xref-enable)

(use-package rscope
  :straight (:host github :repo "rjarzmik/rscope")
  :commands rscope-init rscope-regenerate-database)

(use-package eopengrok
  :straight t
  :commands
  eopengrok-mode eopengrok-find-reference eopengrok-find-text eopengrok-find-definition eopengrok-find-custom
  eopengrok-jump-to-source eopengrok-create-index eopengrok-create-index-with-enable-projects
  :config
  (+nmap! :keymaps 'eopengrok-mode-map
    "n" #'eopengrok-next-line
    "p" #'eopengrok-previous-line
    "q" #'eopengrok-quit
    "RET" #'eopengrok-jump-to-source))



(provide 'me-tags)

;;; me-tags.el ends here
