;;; me-vc.el --- Git and version control -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2022-10-02
;; Last modified: 2025-05-14

;;; Commentary:

;;; Code:

;; It's Magit! A Git Porcelain inside Emacs.
(use-package magit
  :straight t
  :custom
  (magit-diff-refine-hunk t)
  (magit-revision-show-gravatars t)
  (magit-save-repository-buffers nil)
  (magit-format-file-function #'magit-format-file-nerd-icons)
  (magit-display-buffer-function #'magit-display-buffer-fullcolumn-most-v1) ; Show in new window
  :init
  ;; Replace the `project-vc-dir' by `magit-project-status' in project prefix and switch commands
  (with-eval-after-load 'project
    (keymap-set project-prefix-map "v" 'magit-project-status)
    (when-let* ((vc (assoc 'project-vc-dir project-switch-commands)))
      (setcar vc 'magit-project-status)
      (setcdr vc '("Magit project status"))))
  :config
  ;; Automatically refresh Magit after save
  (add-hook 'after-save-hook 'magit-after-save-refresh-status))


;; Edit Git commit messages - part of `magit'
(use-package git-commit
  :after magit
  :commands (global-git-commit-mode)
  :hook (git-commit-setup . +git-commit-insert-commit-prefix)
  :custom
  (git-commit-summary-max-length 72) ; defaults to Github's max commit message length
  (git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line))
  :init
  (global-git-commit-mode 1)
  :config
  ;; Inspired by https://github.com/jesse-c/dotfiles/blob/7788769fc48caffc600409e05be04669c16a71c4/home/dot_config/emacs/init.el#L295
  (defcustom +git-commit-prefix-in-project nil
    "Propose to insert a commit prefix in the current project.

Set this variable in your project's \".dir-locals.el\". You can set it
to `conventional' or `prefix'."
    :group 'minemacs-vc
    :type '(choice (const conventional) (const prefix) (const nil)))

  (make-variable-buffer-local '+git-commit-prefix-in-project)

  (defvar +git-commit-cache-age (* 60 60 24))
  (defvar +git-commit--types nil)
  (defvar +git-commit-format-alist
    `((conventional
       . ((type . ,(rx (seq bol (group-n 1 (+ lower)) (any "(" ":" "!" "+") (* any))))
          (scope . (rx-to-string `(seq bol (or ,@+git-commit--types) "(" (group-n 1 (+ (not ")"))) ")")))
          (scope-sep . ",")
          (format . ("%s%s: " . "(%s)"))))
      (prefix
       . ((types . ,(rx (seq bol "[" (group-n 1 (+ (not (any "]" space)))) (* any))))
          (scope . ,(rx (seq bol "[" (+ (not (any "]" space))) (group-n 1 (+ (not (any "]")))))))
          (scope-sep . " ")
          (format . ("[%s%s] " . " %s"))))))

  (defun +git-commit--cache-file (conv kind)
    "Returns the (filename . bool), the bool means that the cache file is still valid."
    (let* ((cache-dir (expand-file-name ".cache" (or (+project-safe-root) default-directory)))
           (cache-file (expand-file-name (format "%s-%s" conv kind) cache-dir)))
      (unless (file-exists-p cache-dir) (make-directory cache-dir t))
      (cons cache-file
            (and (file-exists-p cache-file)
                 (< (time-to-seconds (time-since (file-attribute-modification-time (file-attributes cache-file))))
                    +git-commit-cache-age)))))

  (defun +git-commit-get-kind (conv kind)
    "CONV can be `conventional' or `prefix', KIND can be `types' or `scopes'."
    (let* ((default-directory (magit-toplevel))
           (cache-file (+git-commit--cache-file conv kind))
           (scopes-p (eq kind 'scopes)))
      (unless (cdr cache-file)
        (with-temp-buffer
          (call-process "git" nil t nil "log" "--pretty=format:%s")
          (goto-char (point-min))
          (let-alist (alist-get conv +git-commit-format-alist)
            (let* ((+git-commit--types (when scopes-p (+git-commit-get-kind conv 'types)))
                   (elements (cl-loop
                              while (let ((case-fold-search nil))
                                      (re-search-forward (eval (if scopes-p .scope .type)) nil 'noerror))
                              append (if scopes-p
                                         (string-split (match-string 1) .scope-sep t "[[:space:]]")
                                       (list (match-string 1))))))
              (with-temp-file (car cache-file) ; Write unique scopes to the cache file
                (insert (string-join (delete-dups elements) "\n")))))))
      (when (file-exists-p (car cache-file))
        (with-temp-buffer
          (insert-file-contents (car cache-file))
          (split-string (buffer-string) "\n" t)))))

  (defun +git-commit-insert-commit-prefix (&optional clean-cache)
    "Prompt for conventional commit type with scope completion."
    (interactive "P")
    (let ((conv +git-commit-prefix-in-project)
          (+git-commit-cache-age (if clean-cache 0 +git-commit-cache-age)))
      (when (and conv
                 (or (called-interactively-p)
                     (and (+first-line-empty-p) ; Skip when amending a commit
                          (y-or-n-p (format "Use %s commit format? " (symbol-name conv))))))
        (let-alist (alist-get conv +git-commit-format-alist)
          (let* ((type (completing-read "Commit type: " (+git-commit-get-kind conv 'types)))
                 (known-scopes (+git-commit-get-kind conv 'scopes))
                 (scopes (and known-scopes (completing-read-multiple "Scope: " known-scopes))))
            (insert (format (car .format) type
                            (let ((s (string-join scopes .scope-sep)))
                              (if (string-empty-p s) "" (format (cdr .format) s)))))
            (let ((pos (point))) (insert "\n") (goto-char pos))))))))


;; Show source files' TODOs (and FIXMEs, etc) in Magit status buffer
(use-package magit-todos
  :straight t)


;; Magit extension for "git-imerge"
(use-package magit-imerge
  :straight t
  :after magit
  :init
  (with-eval-after-load 'transient
    (transient-append-suffix 'magit-merge "m" '("M" "magit-imerge" magit-imerge))))


;; A set of extensions for `magit' to handle multiple repositories simultaneously
(use-package multi-magit
  :straight (:host github :repo "luismbo/multi-magit")
  :init
  (defvar +multi-magit-discover-max-depth 6 "Scan the sub-directoris up to this depth.")
  (defcustom +multi-magit-discover-ignore-directories nil
    "Ignore these regexps when discovering repos.
By default, the regexp is applied to the directory name. If you want to
match the full path, use the \(cons \"path/to/ignore[0-9]*\" full) syntax."
    :type '(repeat (choice regexp (cons regexp (const full)))))
  :commands (+multi-magit-discover-repos)
  :config
  (defun +multi-magit-discover-repos (dir &optional reset remember-projects)
    "Recursively discover and select Git repositories under DIR.

When RESET is non-nil (\\[universal-argument]), reset the
`multi-magit-selected-repositories' and `magit-repository-directories'
variables.

When REMEMBER-PROJECTS (\\[universal-argument] \\[universal-argument]),
use `project-remember-project' with each detected repo."
    (interactive "DSelect the base directory: ")
    (let* ((reset (or reset (= (prefix-numeric-value current-prefix-arg) 4)))
           (remember-projects (or remember-projects (= (prefix-numeric-value current-prefix-arg) 16)))
           (dir (expand-file-name dir))
           (regexps (append +multi-magit-discover-ignore-directories
                            (mapcar #'regexp-quote (append project-vc-extra-root-markers
                                                           (mapcar #'cdr project-vc-backend-markers-alist)))))
           (directories
            (progn
              (message "Creating directory list at %S up to depth %d" dir +multi-magit-discover-max-depth)
              (mapcar #'abbreviate-file-name
                      (seq-filter
                       #'file-directory-p
                       (directory-files-recursively
                        dir
                        directory-files-no-dot-files-regexp
                        t
                        (lambda (path) ; Use a predicate for the maximum depth
                          (and
                           (not (seq-find
                                 (lambda (regexp)
                                   (let ((matching-part (if (and (consp regexp) (eq (car regexp) 'full)) path (file-name-directory path))))
                                     (string-match-p regexp matching-part)))
                                 regexps))
                           (length< (file-name-split (string-remove-prefix dir (expand-file-name path)))
                                    +multi-magit-discover-max-depth)))))))))
      (when reset ; Reset the two variables
        (setq magit-repository-directories
              (cl-delete-if (lambda (dir-depth) (member (car dir-depth) multi-magit-selected-repositories)) magit-repository-directories))
        (setq multi-magit-selected-repositories nil))

      (dolist (dir directories)
        (message "Scanning repositories under: %S" (abbreviate-file-name dir))
        (when-let* ((dir (vc-git-root dir)))
          (when remember-projects (project-remember-project (project-current nil dir)))
          (add-to-list 'multi-magit-selected-repositories dir)))

      (message "Scanning repositories under: done.")

      ;; Set `magit-repository-directories' if not already there
      (dolist (dir multi-magit-selected-repositories)
        (unless (seq-find (lambda (e) (equal (expand-file-name (car e)) dir)) magit-repository-directories)
          (add-to-list 'magit-repository-directories (cons dir 0)))))

    (with-eval-after-load 'desktop
      ;; Save these variables between sessions
      (add-to-list 'desktop-globals-to-save 'magit-repository-directories)
      (add-to-list 'desktop-globals-to-save 'multi-magit-selected-repositories))))


;; Store EIEIO objects using EmacSQL
(use-package closql
  :straight t)


;; Work with Git forges from the comfort of Magit
(use-package forge
  :straight t
  :config
  (require 'on-demand/me-markdown))


;; Emacs-GitLab integration
(use-package lab
  :straight (:host github :repo "isamert/lab.el"))


;; Emacs package for highlighting uncommitted changes
(use-package diff-hl
  :straight t
  :hook (minemacs-first-file . global-diff-hl-mode)
  :hook (diff-hl-mode . +diff-hl-update-on-buffer-change)
  :hook (dired-mode . diff-hl-dired-mode)
  :config
  ;; BUG+HACK: After commiting changes from `magit' and switching back to the
  ;; buffer, the diff-hl doesn't go away until an input happens. This hook will
  ;; ensure updating the `diff-hl' each time we switch to the buffer.
  (defun +diff-hl-update-on-buffer-change ()
    (add-hook 'window-buffer-change-functions (lambda (_win) (diff-hl-update))))
  (diff-hl-flydiff-mode 1))


;; Walk through Git revisions of a file
(use-package git-timemachine
  :straight t
  :hook (git-timemachine-mode . display-line-numbers-mode)
  :config
  (advice-add
   'git-timemachine--show-minibuffer-details :override
   (satch-defun +git-timemachine--show-revision-in-header-line:override-a (revision)
     "Show the current revision in the header-line instead of the echo area."
     (let ((author (if git-timemachine-show-author (concat (nth 6 revision) ": ") ""))
           (sha-or-subject (if (eq git-timemachine-minibuffer-detail 'commit) (car revision) (nth 5 revision))))
       (setq header-line-format
             (format "%s%s [%s (%s)]"
                     (propertize author 'face 'git-timemachine-minibuffer-author-face)
                     (propertize sha-or-subject 'face 'git-timemachine-minibuffer-detail-face)
                     (nth 4 revision) (nth 3 revision)))))))


;; Emacs major modes for Git configuration files
(use-package git-modes
  :straight t
  :mode ("/.dockerignore\\'" . gitignore-mode))


;; Running "repo" from Emacs
(use-package repo
  :straight t
  :hook (repo-mode . +ansi-color-apply-on-buffer))


;; Jujutsu (jj) integration with Emacs `vc' and `project'
(use-package vc-jj
  :straight t
  :when (executable-find "jj")
  :after vc
  :demand)


;; View diffs side-by-side in Emacs
(use-package diffview
  :straight t)


;; A structural diff that understands syntax
(use-package difftastic
  :straight t)


(provide 'me-vc)

;;; me-vc.el ends here
