;;; me-vc.el --- Git and version control -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2022-10-02
;; Last modified: 2025-05-10

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
  :hook (git-commit-setup . +git-commit-insert-conventional-commit)
  :custom
  (git-commit-summary-max-length 72) ; defaults to Github's max commit message length
  (git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line))
  :init
  (global-git-commit-mode 1)
  :config
  ;; Adapted from https://github.com/jesse-c/dotfiles/blob/7788769fc48caffc600409e05be04669c16a71c4/home/dot_config/emacs/init.el#L295
  (defvar +git-commit-conventional-commit-types
    '("feat" "fix" "tweak" "docs" "style" "refactor" "perf" "test" "build" "ci" "chore" "revert"))

  (defvar-local +git-commit-conventional-in-project nil)

  (defun +git-commit--find-conventional-commit-scopes ()
    "Find all scopes used in conventional commits in the current Git project."
    (let* ((default-directory (magit-toplevel))
           (cache-dir (expand-file-name ".cache" (or (+project-safe-root) default-directory)))
           (cache-file (expand-file-name "conventional-commit-scopes" cache-dir)))
      (unless (file-exists-p cache-dir) (make-directory cache-dir t))
      (with-temp-buffer
        (call-process "git" nil t nil "log" "--pretty=format:%s")
        (goto-char (point-min))
        (let ((scopes
               (cl-loop while (re-search-forward
                               (rx-to-string `(seq (or ,@+git-commit-conventional-commit-types) "(" (group-n 2 (+ (not ")"))) ")"))
                               nil t)
                        append (let ((scope-text (match-string 2))) ; Split by comma and add each scope
                                 (split-string scope-text "," t "[ \t]+")))))
          (with-temp-file cache-file ; Write unique scopes to the cache file
            (insert (mapconcat #'identity (delete-dups scopes) "\n")))))
      cache-file)) ; Return the cache file path

  (defun +git-commit-get-conventional-commit-scopes ()
    "Get commit scopes from cache or generate them if needed."
    (let* ((default-directory (magit-toplevel))
           (cache-dir (expand-file-name ".cache" (or (+project-safe-root) default-directory)))
           (cache-file (expand-file-name "conventional-commit-scopes" cache-dir)))
      (unless (and (file-exists-p cache-file)
                   (> (time-to-seconds (time-since (file-attribute-modification-time (file-attributes cache-file))))
                      (* 60 60 24))) ; Cache for 24 hours
        (+git-commit--find-conventional-commit-scopes))
      (when (file-exists-p cache-file)
        (with-temp-buffer
          (insert-file-contents cache-file)
          (split-string (buffer-string) "\n" t)))))

  (defun +git-commit-insert-conventional-commit ()
    "Prompt for conventional commit type with scope completion."
    (interactive)
    (when (and +git-commit-conventional-in-project
               (+first-line-empty-p) ; Skip when amending a commit
               (or (called-interactively-p) (y-or-n-p "Use conventional commit format? ")))
      (let* ((type (completing-read "Commit type: " +git-commit-conventional-commit-types nil t))
             (scopes (+git-commit-get-conventional-commit-scopes))
             (scope-input (completing-read-multiple "Scope: " scopes)))
        (insert type (if (null scope-input) "" (concat "(" (string-join scope-input ",") ")")) ": ")
        (let ((pos (point))) (insert "\n") (goto-char pos))))))


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
     (let* ((date-relative (nth 3 revision))
            (date-full (nth 4 revision))
            (author (if git-timemachine-show-author (concat (nth 6 revision) ": ") ""))
            (sha-or-subject (if (eq git-timemachine-minibuffer-detail 'commit) (car revision) (nth 5 revision))))
       (setq header-line-format
             (format "%s%s [%s (%s)]"
                     (propertize author 'face 'git-timemachine-minibuffer-author-face)
                     (propertize sha-or-subject 'face 'git-timemachine-minibuffer-detail-face)
                     date-full date-relative))))))


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
