;; me-defaults.el --- MinEmacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


;; NOTE: Setting `font-lock-multiline' to 'undecided' org-mode to become unusable:
;; Error during redisplay: (jit-lock-function 22138) signaled (args-out-of-range 0 16341)
(setq-default font-lock-multiline nil)

;;; Better defaults
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(setq default-input-method nil)

;;; Set files and directories for built-in packages
(setq backup-directory-alist (list (cons "." (expand-file-name "backup/" minemacs-local-dir)))
      auto-save-list-file-prefix (expand-file-name "auto-save-list/" minemacs-local-dir))

(setq visible-bell nil ;; set to non-nil to flash!
      ring-bell-function 'ignore
      large-file-warning-threshold (* 50 1024 1024) ;; change to 50 MiB
      use-short-answers t ;; y or n istead of yes or no
      confirm-kill-emacs 'y-or-n-p ;; confirm before quitting
      initial-scratch-message ";; MinEmacs -- start here!"
      frame-resize-pixelwise t
      delete-by-moving-to-trash t)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq auth-sources '("~/.authinfo.gpg") ;; Defaults to GPG
      auth-source-do-cache t
      auth-source-cache-expiry 86400 ; All day, defaut is 2h (7200)
      password-cache t
      password-cache-expiry 86400)

;;; Undo
(setq undo-limit        10000000 ;; 1MB (default is 160kB)
      undo-strong-limit 100000000 ;; 100MB (default is 240kB)
      undo-outer-limit  1000000000) ;; 1GB (default is 24MB)

;;; Editing
(setq display-line-numbers-type 'relative
      tab-always-indent nil
      whitespace-action '(cleanup auto-cleanup))

(setq-default truncate-lines nil
              fill-column 80
              indent-tabs-mode nil
              display-line-numbers-width 3
              tab-width 2)

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
      scroll-preserve-screen-position 'always
      auto-window-vscroll nil
      fast-but-imprecise-scrolling t)

(setq-default scroll-up-aggressively 0.01
              scroll-down-aggressively 0.01)

;; Stretch cursor to the glyph width
(setq x-stretch-cursor t)

(setq window-combination-resize t)

(setq recentf-max-saved-items 100)

;; Mode-line stuff
;; Enable time in the mode-line
(setq display-time-string-forms
      '((propertize (concat 24-hours ":" minutes))))

;;; Enable global modes
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'hs-minor-mode)

;; Guess major mode when saving a file (from Doom Emacs)
(add-hook
 'after-save-hook
 (defun me--guess-file-mode-h ()
   "Guess major mode when saving a file in `fundamental-mode'.

Likely, something has changed since the buffer was opened. e.g. A shebang line
or file path may exist now."
   (when (eq major-mode 'fundamental-mode)
     (let ((buffer (or (buffer-base-buffer) (current-buffer))))
       (and (buffer-file-name buffer)
            (eq buffer (window-buffer (selected-window))) ;; Only visible buffers
            (set-auto-mode))))))

;; From: https://trey-jackson.blogspot.com/2010/04/emacs-tip-36-abort-minibuffer-when.html
(add-hook
 'mouse-leave-buffer-hook
 (defun me-minibuffer--kill-on-mouse-h ()
   "Kill the minibuffer when switching to window with mouse."
   (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
     (abort-recursive-edit))))

(when feat/xwidgets
  ;; Make xwidget-webkit the default browser
  (setq browse-url-browser-function #'xwidget-webkit-browse-url)
  (defalias 'browse-web #'xwidget-webkit-browse-url))

(with-eval-after-load 'minemacs-loaded
  ;; Enable battery (if available) in mode-line
  (me-with-shutup!
   (let ((battery-str (battery)))
     (unless (or (equal "Battery status not available" battery-str)
                 (string-match-p "unknown" battery-str)
                 (string-match-p "N/A" battery-str))
       (display-battery-mode 1)))

   ;; Scroll pixel by pixel
   (pixel-scroll-mode 1)

   ;; Window layout undo/redo (`winner-undo' / `winner-redo')
   (winner-mode 1)

   ;; Precision scroll
   (when (>= emacs-major-version 29)
     (pixel-scroll-precision-mode 1))

   ;; Display time in mode-line
   (display-time-mode 1)

   ;; Highlight current line
   (global-hl-line-mode 1)

   ;; Enable recentf-mode globally
   (recentf-mode 1)

   ;; Enable saving minibuffer history
   (savehist-mode 1)

   ;; Show line and column numbers (cursor position) in mode-line
   (line-number-mode 1)
   (column-number-mode 1)

   ;; Wrap long lines
   (global-visual-line-mode 1)

   ;; Better handling for files with so long lines
   (global-so-long-mode 1)

   ;; Global SubWord mode
   (global-subword-mode 1)))


(provide 'me-defaults)
