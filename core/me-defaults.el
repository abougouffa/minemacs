;; me-defaults.el --- MinEmacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


;;; Why use anything but UTF-8?
(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)

(setq
 ;; ====== Default directories for builtin packages ======
 backup-directory-alist (list (cons "." (+directory-ensure (concat minemacs-local-dir "backup/"))))
 auto-save-list-file-prefix (+directory-ensure (concat minemacs-local-dir "auto-save/"))
 abbrev-file-name (concat minemacs-local-dir "abbrev.el")
 project-list-file (concat minemacs-local-dir "project-list.el")
 tramp-backup-directory-alist backup-directory-alist
 tramp-auto-save-directory (concat minemacs-local-dir "tramp/auto-save/")
 tramp-persistency-file-name (concat minemacs-local-dir "tramp/persistency.el")
 url-configuration-directory (+directory-ensure (concat minemacs-local-dir "url/"))
 url-cookie-file (concat minemacs-local-dir "url/cookie.el")
 url-history-file (concat minemacs-local-dir "url/history.el")
 url-cache-directory (concat minemacs-cache-dir "url.el")
 save-place-file (concat minemacs-local-dir "save-place.el")
 savehist-file (concat minemacs-local-dir "savehist.el")
 org-id-locations-file (concat minemacs-cache-dir "org/id-locations.el")
 org-persist-directory (+directory-ensure (concat minemacs-cache-dir "org/persist/"))
 org-publish-timestamp-directory (+directory-ensure (concat minemacs-cache-dir "org/publish/timestamps/"))
 org-preview-latex-image-directory (+directory-ensure (concat minemacs-cache-dir "org/preview/latex-image/"))
 recentf-save-file (concat minemacs-local-dir "recentf-save.el")
 shared-game-score-directory (+directory-ensure (concat minemacs-local-dir "shared-game-score/"))
 type-break-file-name (concat minemacs-local-dir "type-break.el")
 bookmark-default-file (concat minemacs-local-dir "bookmark.el")
 ede-project-placeholder-cache-file (concat minemacs-local-dir "ede-projects.el")
 kkc-init-file-name (concat minemacs-local-dir "kkc-init-file.el")
 erc-dcc-get-default-directory (+directory-ensure (concat minemacs-local-dir "erc/dcc/"))
 erc-log-channels-directory (+directory-ensure (concat minemacs-local-dir "erc/log-channels/"))
 eshell-directory-name (+directory-ensure (concat minemacs-local-dir "eshell/"))
 eshell-history-file-name (concat minemacs-local-dir "eshell/history.el")
 eshell-last-dir-ring-file-name (concat minemacs-local-dir "eshell/last-dir-ring.el")
 eshell-aliases-file (concat minemacs-local-dir "eshell/aliases")
 eshell-rc-script (concat minemacs-local-dir "eshell/rc")
 eshell-login-script (concat minemacs-local-dir "eshell/login")
 calc-settings-file (concat minemacs-local-dir "calc-settings.el")
 auto-insert-directory (+directory-ensure (concat minemacs-local-dir "auto-insert/"))
 image-dired-dir (+directory-ensure (concat minemacs-local-dir "image-dired/"))
 image-dired-tags-db-file (concat minemacs-local-dir "image-dired/tags-db.el")
 image-dired-temp-rotate-image-file (concat minemacs-cache-dir "image-dired/temp-rotate-image")
 eudc-options-file (concat minemacs-local-dir "eudc-options.el")
 eww-bookmarks-directory (+directory-ensure (concat minemacs-local-dir "eww/bookmarks/"))
 shadow-info-file (concat minemacs-local-dir "shadow/info.el")
 shadow-todo-file (concat minemacs-local-dir "shadow/todo.el")
 semanticdb-default-system-save-directory (concat minemacs-local-dir "semantic/")

 ;; ====== Default behavior ======
 ;; Inhibit startup message
 inhibit-startup-message t
 ;; Inhibit startup message in echo area
 inhibit-startup-echo-area-message (user-login-name) ;; BUG not working!
 ;; Do not ring
 ring-bell-function 'ignore
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
 confirm-kill-emacs 'y-or-n-p
 ;; Filter duplicate entries in kill ring
 kill-do-not-save-duplicates t
 ;; Save existing clipboard text into the kill ring before replacing it.
 save-interprogram-paste-before-kill t
 ;; Save files only in sub-directories of current project
 save-some-buffers-default-predicate 'save-some-buffers-root
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
 ;; Use completing-read interface instead of definitions buffer (needs xref 1.1.0)
 xref-show-definitions-function #'xref-show-definitions-completing-read
 ;; Enable recursive calls to minibuffer
 enable-recursive-minibuffers t
 ;; Kill the shell buffer after exit
 shell-kill-buffer-on-exit t

 ;; ====== Passwords and encryption ======
 ;; Enable password caching
 password-cache t
 ;; One minute, default is 16
 password-cache-expiry 60
 ;; Default auth-sources to GPG
 auth-sources '("~/.authinfo.gpg")
 ;; Enable caching, do not keep asking about GPG key
 auth-source-do-cache t
 ;; All day, default is 2h (7200)
 auth-source-cache-expiry 86400

 ;; ====== Performances ======
 ;; Increase single chunk bytes to read from subprocess (default 4096)
 read-process-output-max (if os/linux
                             (condition-case nil
                                 ;; Android may raise permission-denied error
                                 (with-temp-buffer
                                   (insert-file-contents
                                    "/proc/sys/fs/pipe-max-size")
                                   (string-to-number (buffer-string)))
                               ;; If an error occured, fallback to the default value
                               (error read-process-output-max))
                           (* 1024 1024))

 ;; ====== Aesthetics ======
 ;; Set to non-nil to flash!
 visible-bell nil
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
 ;; No ugly button for widgets
 widget-image-enable nil
 ;; Show unprettified symbol under cursor (when in `prettify-symbols-mode')
 prettify-symbols-unprettify-at-point t

 ;; ====== Undo ======
 ;; 10MB (default is 160kB)
 undo-limit 10000000
 ;; 50MB (default is 240kB)
 undo-strong-limit 50000000
 ;; 150MB (default is 24MB)
 undo-outer-limit 150000000

 ;; ====== Editing ======
 ;; Hitting TAB behavior
 tab-always-indent nil
 ;; Default behavior for `whitespace-cleanup'
 whitespace-action '(cleanup auto-cleanup)
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
 ;; Do not adjust window-vscroll to view tall lines
 auto-window-vscroll nil
 ;; Fast scrolling
 fast-but-imprecise-scrolling t
 ;; Keep the point in the same position while scrolling
 scroll-preserve-screen-position t
 ;; Do not move cursor to the center when scrolling
 scroll-conservatively 101
 ;; Scroll at a margin of one line
 scroll-margin 1
 ;; Better scrolling on Emacs29+, specially on a touchpad
 pixel-scroll-precision-use-momentum t

 ;; ====== Recent files ======
 ;; Increase the maximum number of saved items
 recentf-max-saved-items 100
 ;; Ignore case when searching recentf files
 recentf-case-fold-search t
 ;; Exclude some files from being remembered by recentf
 recentf-exclude
 `(,(rx (* any)
     (or
      "elfeed-db"
      "eln-cache"
      "/cache/"
      ".maildir/"
      ".cache/")
     (* any)
     (? (or "html" "pdf" "tex" "epub")))
   ,(rx "/"
     (or "rsync" "ssh" "tmp" "yadm" "sudoedit" "sudo")
     (* any)))

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
                                 (list ".*" auto-save-list-file-prefix t))

 ;; ====== Compilation ======
 ;; Scroll compilation buffer
 compilation-scroll-output t ; 'first-error ;; good option
 ;; Always kill current compilation process before starting a new one
 compilation-always-kill t
 ;; Skip visited messages on compilation motion commands
 compilation-skip-visited t
 ;; Keep it readable
 compilation-window-height 12)

(setq-default
 ;; ====== Buffer-local variables ======
 ;; Display long lines
 truncate-lines nil
 ;; Default fill column width
 fill-column 80
 ;; Never mix, use only spaces
 indent-tabs-mode nil
 ;; Width for line numbers
 display-line-numbers-width 4
 ;; Small tab is enough!
 tab-width 2)

(with-eval-after-load 'minemacs-loaded
  ;; Ensure creating "session.ID" in a sub-directory
  (with-eval-after-load 'x-win
    (advice-add
     #'emacs-session-filename :filter-return
     (defun +emacs-session-filename--customize-a (filename)
       ;; Create the directory
       (concat minemacs-local-dir "emacs-session/" (file-name-nondirectory filename)))))

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

  (advice-add
   'term-sentinel :around
   (defun +term--kill-after-exit-a (orig-fn proc msg)
     (if (memq (process-status proc) '(signal exit))
         (let ((buffer (process-buffer proc)))
           (apply orig-fn (list proc msg))
           (kill-buffer buffer))
       (apply orig-fn (list proc msg)))))

  ;;; Modes enabled locally, mainly for `prog-mode', `conf-mode' and `text-mode'
  ;; Show line numbers
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
  (add-hook 'conf-mode-hook #'display-line-numbers-mode)
  (add-hook 'text-mode-hook #'display-line-numbers-mode)

  ;; Highlight the current line
  (add-hook 'prog-mode-hook #'hl-line-mode)
  (add-hook 'conf-mode-hook #'hl-line-mode)
  (add-hook 'text-mode-hook #'hl-line-mode)

  ;; Hide/show code blocks, a.k.a. code folding
  (add-hook 'prog-mode-hook #'hs-minor-mode)
  (add-hook 'conf-mode-hook #'hs-minor-mode)

  ;; Wrap long lines
  (add-hook 'prog-mode-hook #'visual-line-mode)
  (add-hook 'conf-mode-hook #'visual-line-mode)
  (add-hook 'text-mode-hook #'visual-line-mode)

  ;;; Other hooks
  ;; Update time stamp (if available) before saving a file
  (add-hook 'before-save-hook 'time-stamp)

  (defvar +whitespace-auto-cleanup-modes
    '(prog-mode conf-mode org-mode markdown-mode
      latex-mode tex-mode bibtex-mode)
    "Enable auto whitespace cleanup before saving for these derived modes.")

  ;; Remove trailing whitespaces on save for some modes
  (add-hook
   'before-save-hook
   (defun +save--whitespace-cleanup-h ()
     (when (cl-some #'derived-mode-p +whitespace-auto-cleanup-modes)
       (whitespace-cleanup))))

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

  ;; Navigate windows using Shift+Direction
  (windmove-default-keybindings)

  ;;; Enable some modes globally
  ;; Enable battery (if available) in mode-line
  (+shutup!
   (let ((battery-str (battery)))
     (unless (or (equal "Battery status not available" battery-str)
                 (string-match-p "unknown" battery-str)
                 (string-match-p "N/A" battery-str))
       (display-battery-mode 1))))

  ;; Window layout undo/redo (`winner-undo' / `winner-redo')
  (winner-mode 1)

  ;; Scroll pixel by pixel, in Emacs29+ there is a more pricise mode way to scroll
  (if (>= emacs-major-version 29)
      (pixel-scroll-precision-mode 1)
    (pixel-scroll-mode 1))

  ;; Display time in mode-line
  (display-time-mode 1)

  ;; Replace selection after start typing
  (delete-selection-mode 1)

  ;; Enable recentf-mode globally
  (+shutup! (recentf-mode 1))

  ;; Show recursion depth in minibuffer (see `enable-recursive-minibuffers')
  (minibuffer-depth-indicate-mode 1)

  ;; Save place in files
  (save-place-mode 1)

  ;; Enable saving minibuffer history
  (savehist-mode 1)

  ;; Auto load files changed on disk
  (global-auto-revert-mode 1)

  ;; Show line number in mode-line
  (line-number-mode 1)

  ;; Show column numbers (cursor position) in mode-line
  (column-number-mode 1)

  ;; Better handling for files with so long lines
  (global-so-long-mode 1)

  ;; Global SubWord mode
  (global-subword-mode 1))


(provide 'me-defaults)
