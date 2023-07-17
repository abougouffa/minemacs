;; me-defaults.el --- MinEmacs defaults for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

;; Inhibit startup message in echo area the brutal way!
;; The `inhibit-startup-echo-area-message' variable is very restrictive, there
;; is only one unique way of setting it right!
;; See: reddit.com/r/emacs/comments/6e9o4o/comment/di8q1t5
(fset 'display-startup-echo-area-message #'ignore)

;;; Why use anything but UTF-8?
(prefer-coding-system 'utf-8)
(set-charset-priority 'unicode)
(set-default-coding-systems 'utf-8)
;; I use mainly English and French. Hence the "Latin-1" which is suitable for
;; major Western Europe languages.
(set-language-environment "Latin-1")
(set-locale-environment "en_US.UTF-8")
;; Use UTF-16-LE in Windows, see: rufflewind.com/2014-07-20/pasting-unicode-in-emacs-on-windows
(set-selection-coding-system (if os/win 'utf-16-le 'utf-8))

(setopt
 ;; ====== Default directories for builtin packages ======
 auto-save-list-file-prefix (+directory-ensure minemacs-local-dir "auto-save/")
 backup-directory-alist (list (cons "." (+directory-ensure minemacs-local-dir "backup/")))
 diary-file (concat minemacs-local-dir "diary")
 custom-theme-directory (concat minemacs-config-dir "themes/")
 ecomplete-database-file (concat minemacs-local-dir "ecomplete-database.el")
 ede-project-placeholder-cache-file (concat minemacs-local-dir "ede-projects.el")
 erc-dcc-get-default-directory (+directory-ensure minemacs-local-dir "erc/dcc/")
 erc-log-channels-directory (+directory-ensure minemacs-local-dir "erc/log-channels/")
 eudc-options-file (concat minemacs-local-dir "eudc-options.el")
 eww-bookmarks-directory (+directory-ensure minemacs-local-dir "eww/bookmarks/")
 fortune-dir (+directory-ensure minemacs-local-dir "fortune/")
 fortune-file (expand-file-name "local" fortune-dir)
 ido-save-directory-list-file (concat minemacs-local-dir "ido-save-directory-list.el")
 kkc-init-file-name (concat minemacs-local-dir "kkc-init.el")
 multisession-dir (concat minemacs-local-dir "multisession/")
 newsticker-cache-filename (concat minemacs-local-dir "newsticker/cache.el")
 newsticker-dir (+directory-ensure minemacs-local-dir "newsticker/data/")
 nsm-settings-file (concat minemacs-local-dir "nsm-settings.el")
 quickurl-url-file (concat minemacs-local-dir "quickurl-url.el")
 rcirc-log-directory (+directory-ensure minemacs-local-dir "rcirc/log/")
 remember-data-directory (+directory-ensure minemacs-local-dir "remember/data/")
 remember-data-file (concat minemacs-local-dir "remember/data.el")
 semanticdb-default-system-save-directory (concat minemacs-local-dir "semantic/")
 shadow-info-file (concat minemacs-local-dir "shadow/info.el")
 shadow-todo-file (concat minemacs-local-dir "shadow/todo.el")
 shared-game-score-directory (+directory-ensure minemacs-local-dir "shared-game-score/")
 srecode-map-save-file (concat minemacs-local-dir "srecode-map.el")
 timeclock-file (concat minemacs-local-dir "timeclock")
 type-break-file-name (concat minemacs-local-dir "type-break.el")

 ;; ====== Additional directories for non-builtin but common packages ======
 pcache-directory (concat minemacs-cache-dir "pcache/")

 ;; ====== Default behavior ======
 ;; Inhibit startup message
 inhibit-startup-screen t
 ;; Do not ring
 ring-bell-function #'ignore
 ;; Set to non-nil to flash!
 visible-bell nil
 ;; Increase the large file threshold to 50 MiB
 large-file-warning-threshold (* 50 1024 1024)
 ;; Initial scratch message (will be overridden if "fortune" is installed)
 initial-scratch-message ";; MinEmacs -- start here!"
 ;; Set initial buffer to fundamental-mode for faster load
 initial-major-mode 'fundamental-mode
 ;; Always prompt in minibuffer (no GUI)
 use-dialog-box nil
 ;; Use y or n instead of yes or no
 use-short-answers t
 ;; Confirm before quitting
 confirm-kill-emacs #'y-or-n-p
 ;; Filter duplicate entries in kill ring
 kill-do-not-save-duplicates t
 ;; Save existing clipboard text into the kill ring before replacing it.
 save-interprogram-paste-before-kill t
 ;; Save files only in sub-directories of current project
 save-some-buffers-default-predicate #'save-some-buffers-root
 ;; Use single space between sentences
 sentence-end-double-space nil
 ;; Move stuff to trash
 delete-by-moving-to-trash t
 ;; Select help window for faster quit!
 help-window-select t
 ;; More info on completions
 completions-detailed t
 ;; Do not ask obvious questions, follow symlinks
 vc-follow-symlinks t
 ;; Display the true file name for symlinks
 find-file-visit-truename t
 ;; Enable recursive calls to minibuffer
 enable-recursive-minibuffers t
 ;; Kill the shell buffer after exit
 shell-kill-buffer-on-exit t
 ;; Don't prompt for confirmation when we create a new file or buffer
 confirm-nonexistent-file-or-buffer nil
 ;; More intuitive buffer naming style
 uniquify-buffer-name-style 'forward

 ;; ====== Performances ======
 ;; Don’t compact font caches during GC
 inhibit-compacting-font-caches t
 ;; Increase single chunk bytes to read from subprocess (default 4096)
 read-process-output-max (if os/linux
                             (condition-case nil
                                 ;; Android may raise permission-denied error
                                 (with-temp-buffer
                                   (insert-file-contents "/proc/sys/fs/pipe-max-size")
                                   (string-to-number (buffer-string)))
                               ;; If an error occured, fallback to the default value
                               (error read-process-output-max))
                           (* 1024 1024))

 ;; ====== Aesthetics and UI ======
 ;; Do force frame size to be a multiple of char size
 frame-resize-pixelwise t
 ;; Stretch cursor to the glyph width
 x-stretch-cursor t
 ;; Resize window combinations proportionally
 window-combination-resize t
 ;; Enable time in the mode-line
 display-time-string-forms '((propertize (concat 24-hours ":" minutes)))
 ;; No ugly button for widgets
 widget-image-enable nil
 ;; Show unprettified symbol under cursor (when in `prettify-symbols-mode')
 prettify-symbols-unprettify-at-point t
 ;; Make tooltips last a bit longer (default 10s)
 tooltip-hide-delay 20
 ;; Use small frames to display tooltips instead of the default OS tooltips
 use-system-tooltips nil
 ;; Animated images loop forever instead of playing the animation only once
 image-animate-loop t
 ;; Set line width for the divider in `window-divider-mode' to 2px
 window-divider-default-bottom-width 2
 window-divider-default-right-width 2

 ;; ====== Undo ======
 ;; 10MB (default is 160kB)
 undo-limit 10000000
 ;; 50MB (default is 240kB)
 undo-strong-limit 50000000
 ;; 150MB (default is 24MB)
 undo-outer-limit 150000000

 ;; ====== Editing ======
 ;; Hitting TAB behavior
 tab-always-indent 'complete
 ;; End files with newline
 require-final-newline t

 ;; ====== Backups ======
 ;; Disable lockfiles
 create-lockfiles nil
 ;; Enable making backup files
 make-backup-files t
 ;; Number each backup file
 version-control t
 ;; Copy instead of renaming current file
 backup-by-copying t
 ;; Clean up after itself
 delete-old-versions t
 ;; Keep up to 5 old versions of each file
 kept-old-versions 5
 ;; Keep up to 5 new versions of each file
 kept-new-versions 5
 ;; Keep up to 5 versions when cleaning a directory
 dired-kept-versions 5

 ;; ====== Scrolling ======
 ;; Do not adjust window-vscroll to view tall lines. Fixes some lag issues see:
 ;; emacs.stackexchange.com/a/28746
 auto-window-vscroll nil
 ;; Fast scrolling
 fast-but-imprecise-scrolling t
 ;; Keep the point in the same position while scrolling
 scroll-preserve-screen-position t
 ;; Do not move cursor to the center when scrolling
 scroll-conservatively 101
 ;; Scroll at a margin of one line
 scroll-margin 1
 ;; The number of lines to scroll
 scroll-step 1
 ;; Columns from the window edge point allowed before horizontal scroll
 hscroll-margin 2
 ;; The number of columns to scroll
 hscroll-step 1

 ;; ====== Auto-Saving, sessions ======
 ;; Enable auto-save (use `recover-file' or `recover-session' to recover)
 auto-save-default t
 ;; Include big deletions
 auto-save-include-big-deletions t
 ;; Set file naming transform
 auto-save-file-name-transforms
 `(;; Prefix tramp autosaves with "tramp-"
   ("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" ,(concat auto-save-list-file-prefix "tramp-\\2") t)
   ;; Local autosaves
   (".*" ,auto-save-list-file-prefix t)))

(setq-default
 ;; ====== Buffer-local variables ======
 ;; Display long lines
 truncate-lines nil
 ;; Default fill column width
 fill-column 80
 ;; Never mix, use only spaces
 indent-tabs-mode nil
 ;; Small tab is enough!
 tab-width 2)

;; ====== Misc hooks and advices ======
;; Kill the minibuffer when switching by mouse to another window.
;; Adapted from: trey-jackson.blogspot.com/2010/04/emacs-tip-36-abort-minibuffer-when.html
(add-hook
 'mouse-leave-buffer-hook
 (defun +minibuffer--kill-on-mouse-h ()
   "Kill the minibuffer when switching to window with mouse."
   (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
     (abort-recursive-edit))))

;; ====== Tweaks on file save ======
;; Guess the major mode after saving a file in `fundamental-mode' (adapted
;; from Doom Emacs).
(add-hook
 'after-save-hook
 (defun +save--guess-file-mode-h ()
   "Guess major mode when saving a file in `fundamental-mode'.
Likely, something has changed since the buffer was opened. e.g. A shebang line
or file path may exist now."
   (when (eq major-mode 'fundamental-mode)
     (let ((buffer (or (buffer-base-buffer) (current-buffer))))
       (and (buffer-file-name buffer)
            (eq buffer (window-buffer (selected-window))) ;; Only visible buffers
            (set-auto-mode))))))

;; ====== Modes enabled locally, mainly for `prog-mode', `conf-mode' and `text-mode' ======
;; Wrap long lines
(+add-hook! (prog-mode conf-mode text-mode) #'visual-line-mode)

;; Show trailing whitespace in `prog-mode' and `conf-mode'
(+add-hook! (prog-mode conf-mode)
  (defun +show-trailing-whitespace-h ()
    (setq-local show-trailing-whitespace t)))

;; When MinEmacs is running in an asynchronous Org export context, there is no
;; need to enable these modes. So we load them only if we haven't been launched
;; through the `me-org-export-async-init' file.
;; All modes and tweaks are enabled after MinEmacs is gets loaded
(+deferred-unless! (featurep 'me-org-export-async-init)
  ;; Navigate windows using Shift+Direction
  (windmove-default-keybindings 'shift)

  ;; ====== Modes enabled globally ======
  ;; Show the battery status (if available) in the mode-line
  (+shutup!
   (let ((battery-str (battery)))
     (unless (or (equal "Battery status not available" battery-str)
                 (string-match-p "unknown" battery-str)
                 (string-match-p "N/A" battery-str))
       (display-battery-mode 1))))

  ;; Fallback the new `fido-vertical-mode' Emacs28+ builtin completion mode if
  ;; the `me-completion' (which contains `vertico-mode' configuration) core
  ;; module is not enabled.
  (unless (and (memq 'me-completion minemacs-core-modules)
               (not (memq 'vertico minemacs-disabled-packages)))
    (fido-vertical-mode 1))

  ;; Window layout undo/redo (`winner-undo' / `winner-redo')
  (winner-mode 1)

  ;; Display divider between windows
  (window-divider-mode 1)

  ;; Display time in mode-line
  (display-time-mode 1)

  ;; Replace selection after start typing
  (delete-selection-mode 1)

  ;; Show recursion depth in minibuffer (see `enable-recursive-minibuffers')
  (minibuffer-depth-indicate-mode 1)

  ;; Show line number in mode-line
  (line-number-mode 1)

  ;; Show column numbers (a.k.a. cursor position) in the mode-line
  (column-number-mode 1)

  ;; Better handling for files with so long lines
  (global-so-long-mode 1)

  ;; Global SubWord mode
  (global-subword-mode 1))


(provide 'me-defaults)

;;; me-defaults.el ends here
