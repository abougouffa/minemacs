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
  :hook (minemacs-first-c/c++-file . +citre--load-default-config-h)
  :commands (+citre-gtags-create-list-of-files-to-index +citre-gtags-create-list-of-files-to-index-bitbake-aware)
  :custom
  ;; Better (!) project root detection function
  (citre-project-root-function #'+citre-recursive-project-root)
  :init
  (defun +citre--load-default-config-h () (require 'citre-config))

  (defcustom +citre-recursive-root-project-detection-files '(".tags" ".repo" ".citre-root")
    "A list of files/directories to use as a project root markers."
    :type '(repeat string)
    :group 'minemacs-prog)

  (defcustom +citre-gtags-recursive-files-list t
    "Find files to index recursively."
    :type 'boolean
    :group 'minemacs-prog)

  (defcustom +citre-gtags-absolete-files-list nil
    "Output absolete pathes in the created files list."
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
    (or (cl-some (apply-partially #'locate-dominating-file (or buffer-file-name default-directory))
                 +citre-recursive-root-project-detection-files) ; locate the root containing the file
        (citre--project-root))) ; Fall back to the default detection!

  (defun +citre-gtags-find-files-command (&optional dir top-dir appendp)
    (let* ((dir (or dir default-directory))
           (top-dir (or top-dir dir))
           (default-directory dir))
      (concat
       (format "echo 'Creating list of files to index in %S ...'\n" dir)
       (find-cmd
        (unless +citre-gtags-recursive-files-list '(maxdepth "1"))
        `(prune (and (type "d") (name ,@+citre-gtags-files-list-ignored-directories)))
        `(iname ,@+citre-gtags-files-list-suffixes)
        '(type "f" "l")
        '(print))
       (unless +citre-gtags-absolete-files-list
         (format " | sed 's|^%s||'" (file-name-as-directory top-dir)))
       (format " %s gtags.files\n" (if appendp ">>" ">")))))

  (defun +citre-gtags-create-list-of-files-to-index (top-dir)
    "Create a list of files to index in TOP-DIR."
    (interactive "DCreate file list in directory: ")
    (let* ((default-directory top-dir))
      (start-process-shell-command "+citre-gtags-files-list" "*+citre-gtags-files-list*" (+citre-gtags-find-files-command))))

  (defun +citre-gtags-create-list-of-files-to-index-bitbake-aware (top-dir build-dir)
    "Create a list of files to index in TOP-DIR and under Bitbake's BUILD-DIR."
    (interactive (list (read-directory-name "Create file list in directory: ")
                       (read-directory-name "Build directory: ")))
    (let* ((default-directory top-dir)
           (+citre-gtags-files-list-ignored-directories
            (append +citre-gtags-files-list-ignored-directories
                    ;; Ignore searching the build directory, the right paths will used from `+bitbake-poky-sources' below
                    (list (file-name-nondirectory (directory-file-name build-dir)) "downloads"))))
      (unless (fboundp '+bitbake-poky-sources)
        (user-error "Make sure you've enabled the `me-embedded' module"))
      (shell-command (+citre-gtags-find-files-command top-dir) "*+citre-gtags-files-list*" "*+citre-gtags-files-list*")
      (dolist (dir (+bitbake-poky-sources build-dir))
        (shell-command (+citre-gtags-find-files-command dir top-dir 'append) "*+citre-gtags-files-list*" "*+citre-gtags-files-list*"))
      (message "Done creating list of files to index."))))


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
  (setq consult--cscope-database-finder #'+consult--cscope-find-database-file)

  ;; Use `+region-or-thing-at-point' for initial input
  (consult-customize
   consult-cscope-file consult-cscope-calling consult-cscope-called-by
   consult-cscope-text consult-cscope-egrep consult-cscope-symbol
   consult-cscope-including consult-cscope-assignment consult-cscope-definition
   :initial (+region-or-thing-at-point)))


;; Reborn Cscope extension for Emacs
(use-package rscope
  :straight (:host github :repo "rjarzmik/rscope")
  :commands (rscope-init rscope-regenerate-database))


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
