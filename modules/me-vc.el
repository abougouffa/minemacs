;;; me-vc.el --- Git and version control -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(use-package magit
  :straight t
  :init
  (+map! :infix "g"
    "g" #'magit-status
    "C" #'magit-clone
    "b" #'magit-blame
    "l" #'magit-log
    "d" #'magit-diff-dwim
    "s" #'magit-stage
    "i" #'magit-init)
  :custom
  (magit-diff-refine-hunk t)
  (magit-revision-show-gravatars t)
  (magit-save-repository-buffers nil)
  ;; Show in new window
  (magit-display-buffer-function #'magit-display-buffer-fullcolumn-most-v1))

(use-package magit-todos
  :straight t
  :after magit
  :demand t
  :config
  (magit-todos-mode 1))

(use-package magit-imerge
  :straight t
  :init
  (with-eval-after-load 'magit
    (transient-append-suffix 'magit-merge "m"
      '("M" "magit-imerge" magit-imerge))))

(use-package closql
  :straight t)

(use-package forge
  :straight t
  :after magit
  :demand t
  :preface
  ;; Keybindings will be overriten by `evil-collection'
  (setq forge-add-default-bindings nil)
  :init
  (+map! :infix "g"
    "f" '(nil :wk "forge")
    "ff" #'forge-dispatch
    "fc" #'forge-create-post
    "fe" #'forge-edit-post
    "ft" #'forge-edit-topic-title
    "fs" #'forge-edit-topic-state
    "fd" #'forge-edit-topic-draft)
  :custom
  (forge-database-file (concat minemacs-local-dir "forge/database.sqlite"))
  :config
  (transient-append-suffix 'forge-dispatch "M"
    '("m" "forge merge (via API)" forge-merge)))

(use-package emojify ;; Needed by `code-review'
  :straight t
  :custom
  (emojify-emoji-set "emojione-v2.2.6")
  (emojify-emojis-dir (concat minemacs-cache-dir "emojify/emojis/"))
  (emojify-display-style 'image)
  (emojify-download-emojis-p t)
  :init
  (when (< emacs-major-version 29)
    (+map! "ie" '(emojify-insert-emoji :wk "Emoji"))))

(use-package code-review
  :straight (:host github :repo "phelrine/code-review" :branch "fix/closql-update")
  :after magit
  :custom
  (code-review-download-dir (concat minemacs-cache-dir "code-review/"))
  (code-review-db-database-file (concat minemacs-local-dir "code-review/database.sqlite"))
  (code-review-log-file (concat minemacs-local-dir "code-review/code-review-error.log"))
  (code-review-auth-login-marker 'forge) ; use the same credentials as forge in ~/.authinfo.gpg
  :init
  (with-eval-after-load 'magit
    (transient-append-suffix 'magit-merge "i"
      '("y" "Review pull-request" code-review-forge-pr-at-point)))
  (with-eval-after-load 'forge
    (transient-append-suffix 'forge-dispatch "c u"
      '("c r" "review pull-request" code-review-forge-pr-at-point))))

(use-package jiralib2
  :straight t
  :commands +jira-insert-ticket-id
  :config
  (defun +jira--ticket-annotation-fn (ticket)
    (let ((item (assoc ticket minibuffer-completion-table)))
      (when item (concat "    " (cdr item)))))

  (defun +jira-insert-ticket-id ()
    "Insert ticket ID for \"open\", \"to do\", or \"in progress\" tickets."
    (interactive)
    (when-let* ((issues (jiralib2-jql-search (format "assignee=\"%s\" AND status in (\"open\",\"to do\",\"in progress\")" jiralib2-user-login-name)))
                (tickets (mapcar (lambda (t) (cons (cdr (assoc 'key t)) (cdr (assoc 'summary (cdr (assoc 'fields t)))))) issues)))
      (insert
       (if (length= tickets 1)
           (car (car tickets))
         (let ((completion-extra-properties '(:annotation-function +jira--ticket-annotation-fn)))
           (completing-read "Select ticket: " tickets)))))))

(use-package diff-hl
  :straight t
  :hook (find-file . diff-hl-mode)
  :hook (dired-mode . diff-hl-dired-mode)
  :hook (vc-dir-mode . diff-hl-dir-mode)
  :hook (diff-hl-mode . diff-hl-flydiff-mode)
  :hook (magit-pre-refresh . diff-hl-magit-pre-refresh)
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :init
  (+map! "gs" #'diff-hl-stage-current-hunk)
  :custom
  (diff-hl-draw-borders nil))

(use-package git-timemachine
  :straight t
  :init
  (+map! "gt" #'git-timemachine-toggle)
  :custom
  (git-timemachine-show-minibuffer-details t))

;; Enforce git commit conventions. See: chris.beams.io/posts/git-commit
(use-package git-commit
  :after magit
  :demand t
  :custom
  (git-commit-summary-max-length 72) ; defaults to Github's max commit message length
  (git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line))
  :config
  (evil-set-initial-state 'git-commit-mode 'insert)
  (global-git-commit-mode 1))

(use-package git-modes
  :straight t
  :init
  :mode ("/.dockerignore\\'" . gitignore-mode))

(use-package repo
  :straight t
  :when (executable-find "repo")
  :init
  (+map! "gr" #'repo-status))

(use-package gee
  :straight (:host bitbucket :repo "olanilsson/gee"))

(use-package diffview
  :straight t
  :init
  (with-eval-after-load 'diff-mode
    (+map-local! :keymaps 'diff-mode-map
      "v" #'diffview-current
      "V" #'diffview-region))
  :config
  (+nvmap! :keymaps 'diffview--mode-map
    "="   #'diffview--align-windows
    "+"   #'diffview--align-windows
    "C-j" #'diffview--next-file
    "C-k" #'diffview--prev-file
    "q"   #'diffview--quit))


(provide 'me-vc)

;;; me-vc.el ends here
