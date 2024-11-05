;;; me-tags.el --- Non-LSP source code tagging tools -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;; Emacs frontend to GNU Global source code tagging system
(use-package ggtags
  :straight t)


;; Ctags IDE on the True Editor!, a superior code reading & auto-completion tool with pluggable backends
(use-package citre
  :straight t
  :after minemacs-first-c/c++-file
  :demand
  :custom
  ;; Better (!) project root detection function
  (citre-project-root-function #'+citre-recursive-project-root)
  :init
  (defcustom +citre-recursive-root-project-detection-files '(".tags" ".repo" ".citre-root")
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
  (require 'citre-config) ; default configuration

  (defun +citre-recursive-project-root ()
    "Search recursively until we find one of `+citre-recursive-root-project-detection-files'.
Fall back to the default `citre--project-root'."
    (or (cl-some (apply-partially #'locate-dominating-file (or buffer-file-name default-directory))
                 +citre-recursive-root-project-detection-files) ; locate the root containing the file
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


;; Cscope interface for Emacs
(use-package xcscope
  :straight t
  :unless os/win
  :commands (cscope-create-list-of-files-to-index cscope-index-files)
  :custom
  (cscope-option-do-not-update-database t)
  (cscope-display-cscope-buffer nil))


;; Cscope integration for Emacs' Consult
(use-package consult-cscope
  :straight (:host github :repo "blorbx/consult-cscope")
  :config
  (defun +consult--cscope-find-database-file (start-dir)
    "Looks first for the dominating directory that includes the database file.
Fallback to the default function if none is found."
    (if-let ((dir (locate-dominating-file start-dir consult-cscope-database-file))
             (not-abs-path (not (file-name-absolute-p consult-cscope-database-file))))
        (expand-file-name consult-cscope-database-file dir)
      (consult--cscope-find-database-file start-dir)))

  ;; Use my modified database finder, particularly useful in "workspaces with
  ;; multiple-projects" (super-projects)
  (setq consult--cscope-database-finder #'+consult--cscope-find-database-file))


;; Clink integration to Emacs
(use-package clink
  :straight (:host github :repo "abougouffa/clink.el")
  :when (+emacs-features-p 'sqlite3)
  :hook (minemacs-first-c/c++-file . global-clink-mode))


;; A client/server indexer for C/C++/Objc[++] with integration for Emacs based on Clang
(use-package rtags
  :straight t
  :custom
  (rtags-use-bookmarks nil)
  (rtags-autostart-diagnostics t)
  (rtags-jump-to-first-match nil)
  (rtags-results-buffer-other-window t)
  ;; Rtags' binaries are renamed on some systems (like Debian)
  (rtags-rc-binary-name (cl-find-if #'executable-find (list rtags-rc-binary-name "rtags-rc")))
  (rtags-rdm-binary-name (cl-find-if #'executable-find (list rtags-rdm-binary-name "rtags-rdm"))))


;; RTags backend for `xref'
(use-package rtags-xref
  :straight t
  :commands (rtags-xref-enable))


;; Reborn Cscope extension for Emacs
(use-package rscope
  :straight (:host github :repo "rjarzmik/rscope")
  :commands (rscope-init rscope-regenerate-database))


;; A C/C++ minor mode for Emacs powered by "libclang"
(use-package irony-mode
  :straight t
  :config
  (when os/win ; Windows performance tweaks
    (when (boundp 'w32-pipe-read-delay) (setq w32-pipe-read-delay 0))
    ;; Set the buffer size to 64K on Windows (from the original 4K)
    (when (boundp 'w32-pipe-buffer-size) (setq irony-server-w32-pipe-buffer-size (* 64 1024)))))


;; Integration of `irony-mode' with `eldoc'
(use-package irony-eldoc
  :straight t)


(provide 'me-tags)

;;; me-tags.el ends here
