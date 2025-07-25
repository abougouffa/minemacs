;;; me-tags.el --- Non-LSP source code tagging tools -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-05-21
;; Last modified: 2025-07-23

;;; Commentary:

;;; Code:

;; Emacs frontend to GNU Global source code tagging system
(use-package ggtags
  :straight t
  :custom
  (ggtags-extra-args (when (or (getenv "GTAGSOBJDIRPREFIX") (getenv "MAKEOBJDIRPREFIX")) '("--objdir")))
  (ggtags-use-sqlite3 t))


;; Ctags IDE on the True Editor!, a superior code reading & auto-completion tool with pluggable backends
(use-package citre
  :straight t
  :commands (+citre-gtags-create-list-of-files-to-index +citre-gtags-create-list-of-files-to-index-bitbake-aware)
  :custom
  (citre-project-root-function #'+citre-dominating-project-root) ; Better (!) project root detection function
  (citre-gtags-args
   `("--compact"
     ;; TEMP: Sane defaults, see: universal-ctags/citre#184
     ,@(when (or (getenv "GTAGSOBJDIRPREFIX") (getenv "MAKEOBJDIRPREFIX"))
         '("--objdir"))))
  (citre-peek-fill-fringe nil) ; don't looks good with `display-line-numbers-mode'
  :init
  (defcustom +citre-auto-enable-ignore-modes '(bash-ts-mode sh-mode)
    "Don't auto-enable `citre-mode' in these modes.
This complements `citre-auto-enable-citre-mode-modes'."
    :type '(repeat symbol)
    :group 'minemacs-prog)

  (defcustom +citre-recursive-root-project-detection-files '(".tags" ".repo" ".citre-root")
    "A list of files/directories to use as a project root markers."
    :type '(repeat string)
    :group 'minemacs-prog)

  (defvar-local +citre-gtags-recursive-files-list t "Find files to index recursively.")
  (defvar-local +citre-gtags-absolete-files-list nil "Output absolete pathes in the created files list.")
  (defvar-local +citre-gtags-files-list-suffixes '("*.[chly]" "*.[ch]xx" "*.[ch]pp" "*.[ch]++" "*.cc" "*.hh")
    "List of filename suffixes globs to index (for extensions for example).")
  (defvar-local +citre-gtags-files-list-ignored-directories '("CVS" "RCS" "SCCS" ".git" ".hg" ".bzr" ".cdv" ".pc" ".svn" ".repo" "_MTN" "_darcs" "_sgbak" "debian")
    "List of directories to be ignored when creating the file list using `+citre-gtags-find-files-command'.")
  :config
  (defvar-keymap +citre-navigation-map
    :doc "Citre navigation commands." :name "citre-navigation"
    "C-p" #'citre-peek
    "C-r" #'citre-peek-reference
    "p"   #'citre-peek-restore
    "j"   #'citre-jump
    "b"   #'citre-jump-back
    "r"   #'citre-jump-to-reference
    "q"   #'citre-query-peek
    "Q"   #'citre-query-jump)
  (keymap-set citre-mode-map "C-z C-p" `("citre-navigation" . ,+citre-navigation-map))

  ;; HACK: Don't enable `citre-mode' in some modes, this fixes the too slow
  ;; `bash-ts-mode' in large code bases
  (advice-add
   'citre-auto-enable-citre-mode :around
   (satch-defun +citre--auto-enable-ignore-modes (fn)
     (unless (and +citre-auto-enable-ignore-modes (derived-mode-p +citre-auto-enable-ignore-modes))
       (funcall fn))))

  ;; BUG: The tilde "~" character cannot be expanded in some Tramp methods (like
  ;; sshfs), causing `citre' to trigger an error when calling
  ;; `citre--tags-file-in-global-cache'. This happens when openning any file of
  ;; a remote project over SSHFS for example.
  (advice-add
   'citre--tags-file-in-global-cache :around
   (satch-defun +citre--tags-file-in-global-cache-no-fail (fn dir)
     (condition-case err
         (let ((inhibit-message t))
           (funcall fn dir))
       (error (+log! "`citre': %s, falling back to a local cache directory" (error-message-string err))
              citre-tags-file-global-cache-dir))))

  ;; Use `citre' with Emacs Lisp, see: https://github.com/universal-ctags/citre/blob/master/docs/user-manual/adapt-an-existing-xref-backend.md
  (defvar +citre-elisp-backend
    (citre-xref-backend-to-citre-backend
     'elisp ; This is the xref backend name
     (lambda () (derived-mode-p 'emacs-lisp-mode)))) ; A function to tell if the backend is usable

  ;; Register the backend, which means to bind it with the symbol `elisp'.
  (citre-register-backend 'elisp +citre-elisp-backend)

  ;; Add Elisp to the backend lists.
  (add-to-list 'citre-find-definition-backends 'elisp)
  (add-to-list 'citre-find-reference-backends 'elisp)

  (defun +citre-dominating-project-root ()
    "Search upward until we find one of `+citre-recursive-root-project-detection-files'.
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
                       (read-directory-name "Bitbake build directory: ")))
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


;; Apply the default configuration (part of `citre')
(use-package citre-config
  :after minemacs-lazy
  :demand)


;; Generate call graph for C/C++ functions
(use-package call-graph
  :straight t)


(provide 'me-tags)

;;; me-tags.el ends here
