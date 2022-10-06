;; -*- lexical-binding: t; -*-


(setq-default font-lock-multiline 'undecided)

;;; Better defaults
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(setq default-input-method nil)

;;; Set files and directories for built-in packages
(setq project-list-file (expand-file-name "projects" minemacs-var-dir)
      recentf-save-file (expand-file-name "recentf" minemacs-var-dir)
      auto-save-list-file-prefix (expand-file-name "autosave/" minemacs-var-dir)
      tramp-auto-save-directory  (expand-file-name "tramp-autosave/" minemacs-var-dir)
      tramp-persistency-file-name (expand-file-name "tramp-persistency" minemacs-var-dir)
      backup-directory-alist (list (cons "." (expand-file-name "backup/" minemacs-var-dir)))
      transient-history-file (expand-file-name "transient/history.el" minemacs-var-dir)
      transient-levels-file (expand-file-name "transient/levels.el" minemacs-var-dir)
      transient-values-file (expand-file-name "transient/values.el" minemacs-var-dir)
      eshell-aliases-file (expand-file-name "eshell/aliases" minemacs-var-dir)
      eshell-directory-name (expand-file-name "eshell/" minemacs-var-dir)
      eshell-history-file-name (expand-file-name "eshell/history" minemacs-var-dir)
      eshell-last-dir-ring-file-name (expand-file-name "eshell/lastdir" minemacs-var-dir))

(setq visible-bell nil ;; set to non-nil to flash!
      ring-bell-function 'ignore
      large-file-warning-threshold 52428800 ;; change to 50 MiB
      use-short-answers t ;; y or n istead of yes or no
      confirm-kill-emacs 'y-or-n-p ;; confirm before quitting
      initial-scratch-message ""
      frame-resize-pixelwise t
      source-directory (expand-file-name "~/Softwares/src/emacs/")
      trash-directory nil ;; Use FreeDesktop.org trashcan (default)
      delete-by-moving-to-trash t)

;;; Undo
(setq undo-limit        10000000 ;; 1MB (default is 160kB)
      undo-strong-limit 100000000 ;; 100MB (default is 240kB)
      undo-outer-limit  1000000000) ;; 1GB (default is 24MB)


;;; Editing
(setq-default display-line-numbers-width 3
              display-line-numbers-type 'relative
              truncate-lines nil
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

;; Stretch cursor to the glyph width
(setq-default x-stretch-cursor t)

(setq-default window-combination-resize t)

;; Mode-line stuff
;; Enable time in the mode-line
(setq display-time-string-forms
      '((propertize (concat 24-hours ":" minutes))))

;;; Enable global modes
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)

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
            (eq buffer (window-buffer (selected-window))) ;; Only visible buffers
            (set-auto-mode))))))

;;; Load fonts at startup, values are read from `me-fonts' if set in config.el,
;; and fallback to `me-default-fonts'
(add-hook 'emacs-startup-hook #'me-set-fonts)

(with-eval-after-load 'minemacs-loaded
  ;; Enable battery (if available) in mode-line
  (me-with-shutup!
   (let ((battery-str (battery)))
     (unless (or (equal "Battery status not available" battery-str)
                 (string-match-p "unknown" battery-str)
                 (string-match-p "N/A" battery-str))
       (display-battery-mode 1)))

   ;; Display time in mode-line
   (display-time-mode 1)

   ;; Highlight current line
   (global-hl-line-mode 1)

   ;; Enable recentf-mode globally
   (recentf-mode 1)

   ;; Global SubWord mode
   (global-subword-mode 1)))


(provide 'me-defaults)
