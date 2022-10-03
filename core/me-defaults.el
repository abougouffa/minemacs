;; -*- lexical-binding: t; -*-


;;; Font
(add-hook
 'emacs-startup-hook
 (lambda ()
   (custom-set-faces
    `(default           ((t (:font "Iosevka Fixed Curly Slab 15"))))
    `(fixed-pitch       ((t (:inherit (default)))))
    `(fixed-pitch-serif ((t (:inherit (default)))))
    `(variable-pitch    ((t (:font "Iosevka Curly Slab 15")))))))

(setq-default font-lock-multiline 'undecided)

;;; Better defaults
(set-default-coding-systems 'utf-8)

;;; Set files and directories for built-in packages
(setq project-list-file (expand-file-name "projects" minemacs-var-dir)
      recentf-save-file (expand-file-name "recentf" minemacs-var-dir)
      auto-save-list-file-prefix (expand-file-name "autosave/" minemacs-var-dir)
      tramp-auto-save-directory  (expand-file-name "tramp-autosave/" minemacs-var-dir)
      backup-directory-alist (list (cons "." (expand-file-name "backup/" minemacs-var-dir)))
      transient-history-file (expand-file-name "transient/history.el" minemacs-var-dir)
      transient-levels-file (expand-file-name "transient/levels.el" minemacs-var-dir)
      transient-values-file (expand-file-name "transient/values.el" minemacs-var-dir))

(setq visible-bell nil ;; set to non-nil to flash!
      ring-bell-function 'ignore
      large-file-warning-threshold 52428800 ;; change to 50 MiB
      use-short-answers t ;; y or n istead of yes or no
      confirm-kill-emacs 'yes-or-no-p ;; confirm before quitting
      initial-scratch-message ";; Scratch"
      frame-resize-pixelwise t
      source-directory (expand-file-name "~/Softwares/src/emacs/")
      trash-directory nil ;; Use FreeDesktop.org trashcan (default)
      delete-by-moving-to-trash t)

;;; Undo
(setq undo-limit        10000000 ;; 1MB   (default is 160kB)
      undo-strong-limit 100000000 ;; 100MB (default is 240kB)
      undo-outer-limit  1000000000) ;; 1GB   (default is 24MB)


;;; Editing
(setq-default display-line-numbers-width 3
              display-line-numbers-type 'relative
              ;;truncate-lines nil
              fill-column 80
              tab-width 2
              indent-tabs-mode nil
              tab-always-indent nil)

;;; Backups
;; Disable backup and lockfiles
(setq create-lockfiles nil
      make-backup-files nil
      version-control t ;; number each backup file
      backup-by-copying t ;; copy instead of renaming current file
      delete-old-versions t ;; clean up after itself
      kept-old-versions 5
      kept-new-versions 5
      tramp-backup-directory-alist backup-directory-alist)

;;; Auto-Saving, sessions...
;; Enable auto-save (use `recover-file' or `recover-session' to recover)
(setq auto-save-default t
      auto-save-include-big-deletions t
      auto-save-file-name-transforms
      (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                  ;; Prefix tramp autosaves to prevent conflicts with local ones
                  (concat auto-save-list-file-prefix "tramp-\\2") t)
            (list ".*" auto-save-list-file-prefix t)))

(setq sentence-end-double-space nil)

;;; Scrolling
(setq hscroll-step 1
      hscroll-margin 0
      scroll-step 1
      scroll-margin 0
      scroll-conservatively 101
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-preserve-screen-position 'always
      auto-window-vscroll nil
      fast-but-imprecise-scrolling t)

;;; Enable global modes
;; Enable line numbering globally adds line numbers to mu4e!
;; (global-display-line-numbers-mode 1)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)

;; Highlight current line
(global-hl-line-mode 1)

;; Enable recentf-mode globally
(recentf-mode 1)

;; Guess major mode when saving a file (from Doom Emacs)
(add-hook
 'after-save-hook
 (defun me-guess-file-mode-h ()
   "Guess major mode when saving a file in `fundamental-mode'.

Likely, something has changed since the buffer was opened. e.g. A shebang line
or file path may exist now."
   (when (eq major-mode 'fundamental-mode)
     (let ((buffer (or (buffer-base-buffer) (current-buffer))))
       (and (buffer-file-name buffer)
            (eq buffer (window-buffer (selected-window))) ; only visible buffers
            (set-auto-mode))))))


(provide 'me-defaults)
