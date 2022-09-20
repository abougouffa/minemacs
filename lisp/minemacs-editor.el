;; -*- lexical-binding: t; -*-

(setq-default display-line-numbers-width 3
              display-line-numbers-type 'relative
              ;;truncate-lines nil
              fill-column 80
              tab-width 2
              indent-tabs-mode nil
              tab-always-indent nil)

;; Disable backup and lockfiles
(setq create-lockfiles nil
      make-backup-files nil
      version-control t ;; number each backup file
      backup-by-copying t ;; copy instead of renaming current file
      delete-old-versions t ;; clean up after itself
      kept-old-versions 5
      kept-new-versions 5
      backup-directory-alist (list (cons "." (concat minemacs-cache-dir "backup/")))
      tramp-backup-directory-alist backup-directory-alist)

;; Enable auto-save (use `recover-file' or `recover-session' to recover)
(setq auto-save-default t
      auto-save-include-big-deletions t
      auto-save-list-file-prefix (concat minemacs-cache-dir "autosave/")
      tramp-auto-save-directory  (concat minemacs-cache-dir "tramp-autosave/")
      auto-save-file-name-transforms
      (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                  ;; Prefix tramp autosaves to prevent conflicts with local ones
                  (concat auto-save-list-file-prefix "tramp-\\2") t)
            (list ".*" auto-save-list-file-prefix t)))

(setq sentence-end-double-space nil)

;; Enable line numbering globally
(global-display-line-numbers-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; Enable recentf-mode globally
(recentf-mode 1)

;; From DOOM
(add-hook
 'after-save-hook
 (defun doom-guess-mode-h ()
   "Guess major mode when saving a file in `fundamental-mode'.

Likely, something has changed since the buffer was opened. e.g. A shebang line
or file path may exist now."
   (when (eq major-mode 'fundamental-mode)
     (let ((buffer (or (buffer-base-buffer) (current-buffer))))
       (and (buffer-file-name buffer)
            (eq buffer (window-buffer (selected-window))) ; only visible buffers
            (set-auto-mode))))))


(provide 'minemacs-editor)
