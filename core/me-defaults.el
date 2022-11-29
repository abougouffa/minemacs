;; me-defaults.el --- MinEmacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


;;; Better defaults
(set-language-environment "UTF-8")
(set-charset-priority 'unicode)
(set-default-coding-systems 'utf-8)

(setq
 ;; ====== Default behavior ======
 ;; Do not ring
 ring-bell-function 'ignore
 ;; Increase the large file threshold to 50 MiB
 large-file-warning-threshold (* 50 1024 1024)
 ;; Initial scratch message (will be overriten if "fortune" is installed)
 initial-scratch-message ";; MinEmacs -- start here!"
 ;; Set initial buffer to fundamental-mode for faster load
 initial-major-mode 'fundamental-mode
 ;; Always prompt in minibuffer (no GUI)
 use-dialog-box nil
 ;; Use y or n istead of yes or no
 use-short-answers t
 ;; Confirm before quitting
 confirm-kill-emacs 'y-or-n-p
 ;; Filter duplicate entries in kill ring
 kill-do-not-save-duplicates t
 ;; Save existing clipboard text into the kill ring before replacing it.
 save-interprogram-paste-before-kill t
 ;; Save files only in subdirs of current project
 save-some-buffers-default-predicate 'save-some-buffers-root
 ;; Use single space between sentences
 sentence-end-double-space nil
 ;; Move stuff to trash
 delete-by-moving-to-trash t
 ;; Select help window to fastly quit!
 help-window-select t
 ;; More info on completions
 completions-detailed t
 ;; Needs xref-1.1.0, for Emacs < 28.1, xref needs to be updated
 ;; Use completing-read interface instead of definitions buffer
 xref-show-definitions-function #'xref-show-definitions-completing-read

 ;; ====== Performances ======
 ;; Increase single chunk bytes to read from subprocess (default 4096)
 read-process-output-max (* 1024 1024)

 ;; ====== Default directories ======
 ;; Default directory to save backups
 backup-directory-alist (list (cons "." (concat minemacs-local-dir "backup/")))
 ;; Locally save backups for files edited via Trump
 tramp-backup-directory-alist backup-directory-alist
 ;; Prefix for auto saved files
 auto-save-list-file-prefix (concat minemacs-local-dir "auto-save-list/")
 ;; Abbrev definitions file
 abbrev-file-name (concat minemacs-local-dir "abbrev-defs")

 ;; ====== Aesthetics ======
 ;; Set to non-nil to flash!
 visible-bell nil
 ;; Better unicode glyph for string truncate
 truncate-string-ellipsis "…"
 ;; Do force frame size to be a multiple of char size
 frame-resize-pixelwise t
 ;; Stretch cursor to the glyph width
 x-stretch-cursor t
 ;; Resize window combinations proportionally
 window-combination-resize t
 ;; Enable time in the mode-line
 display-time-string-forms '((propertize (concat 24-hours ":" minutes)))
 ;; Relative line numbering
 display-line-numbers-type 'relative

 ;; ====== Authentication and encryption ======
 ;; Default auth-sources to GPG
 auth-sources '("~/.authinfo.gpg")
 ;; Enable caching, do not keep asking about GPG key
 auth-source-do-cache t
 ;; All day, default is 2h (7200)
 auth-source-cache-expiry 86400
 ;; Enable password caching
 password-cache t
 ;; One minute, default is 16
 password-cache-expiry 60

 ;; ====== Undo ======
 ;; 1MB (default is 160kB)
 undo-limit 10000000
 ;; 100MB (default is 240kB)
 undo-strong-limit 100000000
 ;; 1GB (default is 24MB)
 undo-outer-limit 1000000000

 ;; ====== Editing ======
 ;; Hitting TAB behavior
 tab-always-indent nil
 ;; Default behavior for `whitespace-cleanup'
 whitespace-action '(cleanup auto-cleanup)

 ;; ====== Backups ======
 ;; Disable backup and lockfiles
 create-lockfiles nil
 ;; Disable making backup files
 make-backup-files nil
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

 ;; ====== Scrolling ======
 ;; Do not adjust window-vscroll to view tall lines
 auto-window-vscroll nil
 ;; Fast scrolling
 fast-but-imprecise-scrolling t
 ;; Keep the point in the same position while scrolling
 scroll-preserve-screen-position t

 ;; ====== Recent files ======
 ;; Increase the maximum number of saved items
 recentf-max-saved-items 100
 recentf-case-fold-search t

 ;; ====== Compilation ======
 ;; Scroll compilation buffer until first error
 compilation-scroll-output 'first-error
 ;; Don't need enter
 compilation-read-command nil
 ;; Keep it readable
 compilation-window-height 12

 ;; ====== Timestamps ======
 ;; Do enable time-stamps
 time-stamp-active t
 ;; Check the first 12 buffer lines for Time-stamp: <>
 time-stamp-line-limit 12
 ;; Timestamp format
 time-stamp-format "%04Y-%02m-%02d %02H:%02M:%02S"

 ;; ====== Auto-Saving, sessions ======
 ;; Enable auto-save (use `recover-file' or `recover-session' to recover)
 auto-save-default t
 ;; Include big deletions
 auto-save-include-big-deletions t
 ;; Set file naming transform
 auto-save-file-name-transforms (list
                                 ;; Prefix tramp autosaves with "tramp-"
                                 (list
                                  "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                                  (concat auto-save-list-file-prefix "tramp-\\2") t)
                                 ;; Local autosaves
                                 (list ".*" auto-save-list-file-prefix t)))

(setq-default
 ;; ====== Editing ======
 ;; Display long lines
 truncate-lines nil
 ;; Default fill column width
 fill-column 80
 ;; Never mix, use only spaces
 indent-tabs-mode nil
 ;; Width for line numbers
 display-line-numbers-width 4
 ;; Small tab is enough!
 tab-width 2
 ;; End files with newline
 require-final-newline t

 ;; ======= Scrolling =======
 ;; Do not scroll to the center when point exceeds the beginning/end of buffer.
 scroll-up-aggressively 0.01
 scroll-down-aggressively 0.01)

;;; Enable `display-line-numbers-mode' in `prog-mode' and `text-mode'
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)

;; Highlight the current line in `prog-mode' and `text-mode'
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)

;; Update time stamp when saving a file
(add-hook 'before-save-hook 'time-stamp)

;; Remove trailing whitespaces on save, for modes derived
;; from `prog-mode', `org-mode' or `markdown-mode'
(add-hook
 'before-save-hook
 (defun +save--delete-trailing-whitespace-h ()
   (when (or (derived-mode-p 'prog-mode)
             (derived-mode-p 'org-mode)
             (derived-mode-p 'markdown-mode))
     (delete-trailing-whitespace))))

;; Guess major mode when saving a file (adapted from Doom Emacs)
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

;; Kill minibuffer when switching by mouse to another window
;; Taken from: https://trey-jackson.blogspot.com/2010/04/emacs-tip-36-abort-minibuffer-when.html
(add-hook
 'mouse-leave-buffer-hook
 (defun +minibuffer--kill-on-mouse-h ()
   "Kill the minibuffer when switching to window with mouse."
   (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
     (abort-recursive-edit))))

;; Close compilation buffer if succeeded without warnings
;; Adapted from: http://stackoverflow.com/questions/11043004/emacs-compile-buffer-auto-close
(add-hook
 'compilation-finish-functions
 (defun +compilation--bury-if-successful-h (buf str)
   "Bury a compilation buffer if succeeded without warnings."
   (when (and
          (string-match "compilation" (buffer-name buf))
          (string-match "finished" str)
          (not (with-current-buffer buf
                 (save-excursion
                   (goto-char (point-min))
                   (search-forward "warning" nil t)))))
     (run-with-timer
      2 nil
      (lambda (b)
        (with-selected-window (get-buffer-window b)
          (kill-buffer-and-window))) buf))))

(when feat/xwidgets
  ;; Make xwidget-webkit the default browser
  (setq browse-url-browser-function #'xwidget-webkit-browse-url)
  (defalias 'browse-web #'xwidget-webkit-browse-url))

(with-eval-after-load 'minemacs-loaded
  ;; Enable battery (if available) in mode-line
  (+shutup!
   (let ((battery-str (battery)))
     (unless (or (equal "Battery status not available" battery-str)
                 (string-match-p "unknown" battery-str)
                 (string-match-p "N/A" battery-str))
       (display-battery-mode 1))))

  ;; Scroll pixel by pixel
  (pixel-scroll-mode 1)

  ;; Window layout undo/redo (`winner-undo' / `winner-redo')
  (winner-mode 1)

  ;; Precision scroll
  (when (>= emacs-major-version 29)
    (pixel-scroll-precision-mode 1))

  ;; Display time in mode-line
  (display-time-mode 1)

  ;; Replace selection after start typing
  (delete-selection-mode 1)

  ;; Enable recentf-mode globally
  (recentf-mode 1)

  ;; Save place in files
  (save-place-mode 1)

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
  (global-subword-mode 1))


(provide 'me-defaults)
