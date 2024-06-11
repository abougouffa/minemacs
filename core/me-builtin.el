;;; me-builtin.el --- Emacs built-in packages -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package emacs
  :hook (after-save . +save--guess-file-mode-h)
  :custom
  ;; ====== Default directories for builtin packages ======
  (auto-save-list-file-prefix (+directory-ensure minemacs-local-dir "auto-save/"))
  (backup-directory-alist (list (cons "." (+directory-ensure minemacs-local-dir "backup/"))))
  (custom-theme-directory (concat minemacs-config-dir "themes/"))
  (diary-file (concat minemacs-local-dir "diary"))
  (ecomplete-database-file (concat minemacs-local-dir "ecomplete-database.el"))
  (ede-project-placeholder-cache-file (concat minemacs-local-dir "ede-projects.el"))
  (erc-dcc-get-default-directory (+directory-ensure minemacs-local-dir "erc/dcc/"))
  (erc-log-channels-directory (+directory-ensure minemacs-local-dir "erc/log-channels/"))
  (eudc-options-file (concat minemacs-local-dir "eudc-options.el"))
  (eww-bookmarks-directory (+directory-ensure minemacs-local-dir "eww/bookmarks/"))
  (fortune-dir (+directory-ensure minemacs-local-dir "fortune/"))
  (fortune-file (expand-file-name "local" fortune-dir))
  (ido-save-directory-list-file (concat minemacs-local-dir "ido-save-directory-list.el"))
  (kkc-init-file-name (concat minemacs-local-dir "kkc-init.el"))
  (multisession-dir (concat minemacs-local-dir "multisession/"))
  (newsticker-cache-filename (concat minemacs-local-dir "newsticker/cache.el"))
  (newsticker-dir (+directory-ensure minemacs-local-dir "newsticker/data/"))
  (nsm-settings-file (concat minemacs-local-dir "nsm-settings.el"))
  (quickurl-url-file (concat minemacs-local-dir "quickurl-url.el"))
  (rcirc-log-directory (+directory-ensure minemacs-local-dir "rcirc/log/"))
  (remember-data-directory (+directory-ensure minemacs-local-dir "remember/data/"))
  (remember-data-file (concat minemacs-local-dir "remember/data.el"))
  (semanticdb-default-system-save-directory (concat minemacs-local-dir "semantic/"))
  (shadow-info-file (concat minemacs-local-dir "shadow/info.el"))
  (shadow-todo-file (concat minemacs-local-dir "shadow/todo.el"))
  (shared-game-score-directory (+directory-ensure minemacs-local-dir "shared-game-score/"))
  (srecode-map-save-file (concat minemacs-local-dir "srecode-map.el"))
  (timeclock-file (concat minemacs-local-dir "timeclock"))
  (type-break-file-name (concat minemacs-local-dir "type-break.el"))
  (viper-custom-file-name (concat minemacs-local-dir "viper.el"))

  ;; ====== Better defaults ======
  (auto-save-default t) ; Enable auto-save (use `recover-file' or `recover-session' to recover)
  (auto-save-include-big-deletions t) ; Include big deletions
  (auto-save-file-name-transforms ; Set file naming transform for `auto-save'
   `(;; Prefix tramp autosaves with "tramp-"
     ("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" ,(concat auto-save-list-file-prefix "tramp-\\2") t)
     ;; Local autosaves
     (".*" ,auto-save-list-file-prefix t)))
  (auto-window-vscroll nil) ; Do not adjust window-vscroll to view tall lines. Fixes some lag issues: emacs.stackexchange.com/a/28746
  (fast-but-imprecise-scrolling t) ; Fast scrolling
  (scroll-preserve-screen-position t) ; Keep the point in the same position while scrolling
  (scroll-conservatively 101) ; Do not move cursor to the center when scrolling
  (scroll-margin 1) ; Scroll at a margin of one line
  (scroll-step 1) ; The number of lines to scroll
  (hscroll-margin 2) ; Columns from the window edge point allowed before horizontal scroll
  (hscroll-step 1) ; The number of columns to scroll
  (create-lockfiles nil) ; Disable lockfiles
  (make-backup-files t) ; Enable making backup files
  (version-control t) ; Number each backup file
  (backup-by-copying t) ; Copy instead of renaming current file
  (delete-old-versions t) ; Clean up after itself
  (kept-old-versions 5) ; Keep up to 5 old versions of each file
  (kept-new-versions 5) ; Keep up to 5 new versions of each file
  (dired-kept-versions 5) ; Keep up to 5 versions when cleaning a directory
  (tab-always-indent nil) ; Make TAB indents first, then inserts the TAB character
  (tab-first-completion 'word) ; TAB completion behavior
  (require-final-newline t) ; End files with newline
  (undo-limit 10000000) ; 10MB (default is 160kB)
  (undo-strong-limit 50000000) ; 50MB (default is 240kB)
  (undo-outer-limit 150000000) ; 150MB (default is 24MB)
  (use-system-tooltips nil) ; Use small frames to display tooltips instead of the default OS tooltips
  (window-combination-resize t) ; Resize window combinations proportionally
  (x-stretch-cursor t) ; Stretch cursor to the glyph width
  (frame-resize-pixelwise t) ; Do force frame size to be a multiple of char size
  (inhibit-compacting-font-caches t) ; Don’t compact font caches during GC
  (read-process-output-max ; Increase single chunk bytes to read from subprocess (default 4096)
   (if os/linux
       (condition-case nil
           ;; Android may raise permission-denied error
           (with-temp-buffer
             (insert-file-contents "/proc/sys/fs/pipe-max-size")
             (string-to-number (buffer-string)))
         ;; If an error occurred, fallback to the default value
         (error read-process-output-max))
     (* 1024 1024)))
  (confirm-nonexistent-file-or-buffer nil) ; Don't prompt for confirmation when we create a new file or buffer
  (enable-recursive-minibuffers t) ; Enable recursive calls to minibuffer
  (completion-ignore-case t) ; Ignore case when completing
  (read-buffer-completion-ignore-case t)
  (find-file-visit-truename t) ; Display the true file name for symlinks
  (sentence-end-double-space nil) ; Use single space between sentences
  (delete-by-moving-to-trash t) ; Move stuff to trash
  (save-some-buffers-default-predicate #'save-some-buffers-root) ; Save files only in sub-directories of current project
  (inhibit-startup-screen t) ; Inhibit startup message
  (ring-bell-function #'ignore) ; Do not ring
  (visible-bell nil) ; Set to non-nil to flash!
  (large-file-warning-threshold (* 50 1024 1024)) ; Increase the large file threshold to 50 MiB
  (initial-scratch-message nil) ; No initial scratch message
  (initial-major-mode 'fundamental-mode) ; Set initial buffer to fundamental-mode for faster load
  (use-dialog-box nil) ; Always prompt in minibuffer (no GUI)
  (use-short-answers t) ; Use y or n instead of yes or no
  (confirm-kill-emacs #'y-or-n-p) ; Confirm before quitting
  (prettify-symbols-unprettify-at-point t) ; Show unprettified symbol under cursor (when in `prettify-symbols-mode')
  (display-fill-column-indicator-character ?\u250a) ; Use a dashed line for `display-fill-column-indicator-mode'
  (apropos-do-all t) ; Make apropos commands search more extensively
  (vc-follow-symlinks t) ; Do not ask obvious questions, follow symlinks
  (shell-kill-buffer-on-exit t) ; Kill the shell buffer after exit
  (uniquify-buffer-name-style 'forward) ; More intuitive buffer naming style
  (widget-image-enable nil) ; No ugly button for widgets
  (tooltip-hide-delay 20) ; Make tooltips last a bit longer (default 10s)
  (image-animate-loop t) ; Animated images loop forever instead of playing the animation only once
  :init
  (setq-default truncate-lines nil ; Display long lines
                fill-column 80 ; Default fill column width
                tab-width 4 ; Default (8) is too big!
                cursor-type 'bar) ; Use a bar by default, this can be changed by other modes (`evil', `god-mode', etc.)

  ;; Inhibit startup message in echo area the brutal way!
  ;; The `inhibit-startup-echo-area-message' variable is very restrictive, there is only one unique way of setting it right!
  ;; See: reddit.com/r/emacs/comments/6e9o4o/comment/di8q1t5
  (fset 'display-startup-echo-area-message #'ignore)
  (fset 'display-startup-screen #'ignore)

  ;;; Why use anything but UTF-8?
  (prefer-coding-system 'utf-8)
  (set-charset-priority 'unicode)
  (set-default-coding-systems 'utf-8)
  ;; I use mainly English and French. Hence the "Latin-1" which is suitable for major Western Europe languages.
  (set-language-environment "Latin-1")
  (set-locale-environment "en_US.UTF-8")
  ;; Use UTF-16-LE in Windows, see: rufflewind.com/2014-07-20/pasting-unicode-in-emacs-on-windows
  (set-selection-coding-system (if os/win 'utf-16-le 'utf-8))
  :config
  ;; Make `ESC' behave like `C-g'
  (keymap-global-set "<escape>" #'keyboard-escape-quit)

  ;; Unbind some annoying commands
  (keymap-global-unset "C-z" 'remove)
  (keymap-global-unset "C-x C-z" 'remove)

  ;; Remap some keys/page
  (keymap-global-set "C-c f" #'recentf) ; Instead of `find-file-read-only'
  (keymap-global-set "C-c D" #'+delete-this-file-and-buffer)
  (keymap-global-set "C-x k" #'kill-current-buffer) ; Instead of `kill-buffer'
  (keymap-global-set "<f1>" #'shell) ; Instead of `help-map' (accessible via `C-h')
  (keymap-global-set "C-w" #'+kill-region-or-backward-word) ; Instead of `kill-region'
  (keymap-global-set "<remap> <kill-region>" #'+kill-region-or-backward-word) ; C-w
  (keymap-global-set "<remap> <kill-word>" #'+kill-whitespace-or-word) ; M-d
  (keymap-global-set "<remap> <backward-kill-word>" #'+backward-kill-whitespace-or-word) ; M-delete or C-backspace

  (defvar-keymap minemacs-open-thing-map
    :doc "Open thing, under `C-c o'."
    :name "Open thing")

  (keymap-global-set "C-c o" minemacs-open-thing-map)

  ;; Disable previously enabled custom themes before enabling a new one.
  (advice-add
   'load-theme :before
   (satch-defun +theme--disable-previous-themes:before-a (&rest _)
     "Disable previously enabled themes before enabling the new one."
     (mapc #'disable-theme custom-enabled-themes)))

  (advice-add
   'load-theme :after
   (satch-defun +theme--save-current-theme-background-color:after-a (&rest _)
     "Save the background color."
     (with-temp-buffer
       (insert (face-background 'default))
       (let ((buffer-file-name (concat minemacs-cache-dir "background-color")))
         (+shutup! (basic-save-buffer-1))))))

  ;; Show trailing whitespace in `prog-mode' and `conf-mode'
  (+setq-hook! (prog-mode conf-mode) show-trailing-whitespace t)

  ;; Guess the major mode after saving a file in `fundamental-mode' (adapted from Doom Emacs).
  (defun +save--guess-file-mode-h ()
    "Guess major mode when saving a file in `fundamental-mode'.
Likely, something has changed since the buffer was opened. e.g. A shebang line
or file path may exist now."
    (when (eq major-mode 'fundamental-mode)
      (let ((buffer (or (buffer-base-buffer) (current-buffer))))
        (and (buffer-file-name buffer)
             (eq buffer (window-buffer (selected-window))) ;; Only visible buffers
             (set-auto-mode)))))

  ;; Advice `emacs-session-filename' to ensure creating "session.ID" files in a sub-directory
  (let ((x-win-dir (+directory-ensure minemacs-local-dir "x-win/")))
    (advice-add
     'emacs-session-filename :filter-return
     (satch-defun +emacs-session-filename--in-subdir:filter-return-a (session-filename)
       "Put the SESSION-FILENAME in the \"x-win/\" sub-directory."
       (concat x-win-dir (file-name-nondirectory session-filename))))

    ;; Don't show session files in recentf list and so on
    (+ignore-root x-win-dir))

  ;; Offer to create parent directories if they do not exist
  ;; https://github.com/cjohansen/.emacs.d/blob/master/settings/sane-defaults.el
  (defun +create-non-existent-directory ()
    (let ((parent-directory (file-name-directory buffer-file-name)))
      (when (and (not (file-exists-p parent-directory))
                 (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
        (make-directory parent-directory t))))

  (add-to-list 'find-file-not-found-functions #'+create-non-existent-directory))

(use-package minibuffer
  :hook (minibuffer-setup . cursor-intangible-mode)
  :custom
  ;; Ignores case when completing files names
  (read-file-name-completion-ignore-case t)
  ;; More info on completions
  (completions-detailed t)
  ;; Do not allow the cursor in the minibuffer prompt (goes with `cursor-intangible-mode')
  (minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt)))

(use-package crm
  :config
  ;; From: https://github.com/a-schaefers/spartan-emacs/blob/main/spartan-layers/spartan-vertico.el
  ;; Add prompt indicator to `completing-read-multiple'. We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (advice-add
   #'completing-read-multiple :filter-args
   (satch-defun +crm--indicator:filter-args-a (args)
     (cons (format "[CRM%s] %s" (replace-regexp-in-string "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" "" crm-separator) (car args)) (cdr args)))))

(use-package transient
  :straight t
  :autoload transient-define-prefix transient-define-infix transient-define-suffix
  ;; Map ESC and q to quit transient
  :bind (:map
         transient-map
         ("q" . transient-quit-one)
         ("<escape>" . transient-quit-one)))

(use-package tramp
  :straight t
  :init
  ;; This is faster than the default "scp"
  (unless os/win
    (setq tramp-default-method "ssh"))
  ;; HACK: Setting `tramp-persistency-file-name' in `:custom' is not working properly!
  (setq tramp-persistency-file-name (concat minemacs-local-dir "tramp/persistency.el"))
  :custom
  (tramp-auto-save-directory (concat minemacs-local-dir "tramp/auto-save/"))
  (tramp-backup-directory-alist backup-directory-alist)
  (tramp-default-remote-shell "/bin/bash"))

(use-package password-cache
  :custom
  (password-cache t) ; Enable password caching
  (password-cache-expiry 60)) ; One minute, default is 16

(use-package auth-source
  :custom
  (auth-sources '("~/.authinfo.gpg")) ; Default auth-sources to GPG
  (auth-source-do-cache t) ; Enable caching, do not keep asking about GPG key
  (auth-source-cache-expiry 86400)) ; All day, default is 2h (7200)

(use-package epa
  :custom
  ;; Force gpg-agent to use minibuffer to prompt for passphrase (GPG 2.1+).
  (epg-pinentry-mode 'loopback))

(use-package epa-file
  :after minemacs-first-file
  :demand
  :config
  (+shutup! (epa-file-enable)))

(use-package dired
  ;; Enable adding mail attachments from dired "C-c RET C-a" for
  ;; `gnus-dired-attach'
  :hook (dired-mode . turn-on-gnus-dired-mode)
  :custom
  (dired-dwim-target t)
  (dired-auto-revert-buffer t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (dired-clean-confirm-killing-deleted-buffers nil))

(use-package dired-aux
  :custom
  (dired-vc-rename-file t)
  (dired-create-destination-dirs 'ask))

(use-package dired-x
  :hook (dired-mode . dired-omit-mode)
  :custom
  (dired-omit-verbose nil)
  :config
  (cl-callf concat dired-omit-files
    "\\|^\\.\\(?:svn\\|git\\|hg\\|repo\\)\\'"
    "\\|^\\.DS_Store\\'"
    "\\|^flycheck_.*"
    "\\|^\\.ccls-cache\\'"
    "\\|^\\.tags\\'"
    "\\|\\(?:\\.js\\)?\\.meta\\'"
    "\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'")

  ;; Open some files with OS' default application
  (when-let (cmd (cond ((or os/linux os/bsd) "xdg-open") (os/mac "open") (os/win "start")))
    (setq dired-guess-shell-alist-user
          `(("\\.\\(?:docx\\|pdf\\|djvu\\|eps\\)\\'" ,cmd)
            ("\\.\\(?:jpe?g\\|png\\|gif\\|xpm\\)\\'" ,cmd)
            ("\\.\\(?:xcf\\)\\'" ,cmd)
            ("\\.csv\\'" ,cmd)
            ("\\.tex\\'" ,cmd)
            ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
            ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
            ("\\.html?\\'" ,cmd)
            ("\\.md\\'" ,cmd)))))

(use-package doc-view
  :custom
  (doc-view-continuous t)
  (doc-view-mupdf-use-svg (+emacs-features-p 'rsvg)))

(use-package project
  :straight t
  :commands project-remember-projects-under
  :hook (kill-emacs . +project-forget-zombie-projects)
  :custom
  (project-list-file (concat minemacs-local-dir "project-list.el"))
  (project-vc-extra-root-markers '(".projectile.el" ".project.el" ".project"))
  :bind (("C-x p a" . +project-add-project)))

(use-package tab-bar
  :hook (minemacs-lazy . tab-bar-mode)
  :custom
  (tab-bar-format '(tab-bar-format-history tab-bar-format-tabs tab-bar-separator))
  (tab-bar-tab-name-format-function #'+tab-bar-tab-spaced-name-format)
  (tab-bar-close-button-show nil)
  (tab-bar-auto-width-max '(150 20))
  (tab-bar-tab-hints t)
  (tab-bar-show nil)
  :config
  (defun +tab-bar-tab-spaced-name-format (tab i)
    (let ((current-p (eq (car tab) 'current-tab)))
      (propertize
       (concat (if tab-bar-tab-hints (format " %c " (+ ?❶ (1- i))) "")
               (alist-get 'name tab)
               (or (and tab-bar-close-button-show
                        (not (eq tab-bar-close-button-show
                                 (if current-p 'non-selected 'selected)))
                        tab-bar-close-button)
                   ""))
       'face (funcall tab-bar-tab-face-function tab))))

  (with-eval-after-load 'nerd-icons
    (setq tab-bar-close-button
          (propertize (concat (nerd-icons-faicon "nf-fa-close" :height 0.5) " ")
                      'close-tab t :help "Click to close tab"))))

(use-package flymake
  :straight t
  :hook ((prog-mode conf-mode) . flymake-mode)
  :custom
  (flymake-fringe-indicator-position 'right-fringe)
  (flymake-error-bitmap '(+flymake-bitmap-left-arrow-hi-res compilation-error))
  (flymake-warning-bitmap '(+flymake-bitmap-left-arrow-hi-res compilation-warning))
  (flymake-note-bitmap '(+flymake-bitmap-left-arrow-hi-res compilation-info))
  :config
  (transient-define-prefix +flymake-transient ()
    "Transient for flymake."
    [[("n" "Next error" flymake-goto-next-error :transient t)
      ("N" "Prev error" flymake-goto-prev-error :transient t)]
     [("B" "Buffer diagnostics" flymake-show-buffer-diagnostics :transient t)
      ("P" "Project diagnostics" flymake-show-project-diagnostics :transient t)
      ("L" "Log buffer" flymake-switch-to-log-buffer :transient t)]
     [("S" "Start" flymake-start :transient t)
      ("Q" "Quit" ignore :transient t)]])

  ;; Use the session's `load-path' with flymake
  (with-eval-after-load 'elisp-mode
    (cl-callf append elisp-flymake-byte-compile-load-path load-path))

  ;; Larger right frings
  (with-eval-after-load 'fringe
    (set-fringe-style '(8 . 13)))

  ;; Better fringe bitmaps
  (define-fringe-bitmap '+flymake-bitmap-left-arrow-hi-res
    [#b00000011110
     #b00000111100
     #b00001111000
     #b00011110000
     #b00111100000
     #b01111000000
     #b01111000000
     #b00111100000
     #b00011110000
     #b00001111000
     #b00000111100
     #b00000011110]
    nil 13))

(use-package xt-mouse
  :hook (tty-setup . xterm-mouse-mode))

(use-package eshell
  :custom
  (eshell-aliases-file (concat minemacs-local-dir "eshell/aliases"))
  (eshell-directory-name (+directory-ensure minemacs-local-dir "eshell/"))
  (eshell-history-file-name (concat minemacs-local-dir "eshell/history.el"))
  (eshell-last-dir-ring-file-name (concat minemacs-local-dir "eshell/last-dir-ring.el"))
  (eshell-login-script (concat minemacs-local-dir "eshell/login"))
  (eshell-rc-script (concat minemacs-local-dir "eshell/rc"))
  (eshell-scroll-to-bottom-on-input 'this))

(use-package reftex ;; Inspired by Doom Emacs
  :hook (reftex-toc-mode . reftex-toc-rescan)
  :custom
  ;; Get RefTeX working with BibLaTeX. See: tex.stackexchange.com/a/31992/43165
  (reftex-cite-format
   '((?a . "\\autocite[]{%l}")
     (?b . "\\blockcquote[]{%l}{}")
     (?c . "\\cite[]{%l}")
     (?f . "\\footcite[]{%l}")
     (?n . "\\nocite{%l}")
     (?p . "\\parencite[]{%l}")
     (?s . "\\smartcite[]{%l}")
     (?t . "\\textcite[]{%l}"))
   ;; This is needed when `reftex-cite-format' is set. See:
   ;; superuser.com/a/1386206
   (reftex-plug-into-AUCTeX t)
   (reftex-toc-split-windows-fraction 0.3)))

(use-package bibtex
  :hook (bibtex-mode . display-line-numbers-mode)
  :custom
  (bibtex-dialect 'biblatex)
  (bibtex-align-at-equal-sign t)
  (bibtex-text-indentation 20))

(use-package treesit
  :when (+emacs-features-p 'tree-sitter)
  :custom
  (treesit-font-lock-level 4))

(use-package dockerfile-ts-mode
  :when (+emacs-features-p 'tree-sitter)
  :mode "/Dockerfile\\'")

(use-package cmake-ts-mode
  :when (+emacs-features-p 'tree-sitter)
  :mode "CMakeLists\\.txt\\'"
  :mode "\\.cmake\\'")

(use-package autoinsert
  :custom
  (auto-insert-directory (+directory-ensure minemacs-local-dir "auto-insert/")))

(use-package hideif
  :custom
  (hide-ifdef-shadow t)
  (hide-ifdef-initially t))

(use-package hl-line
  ;; Highlight the current line
  :hook ((prog-mode conf-mode text-mode) . hl-line-mode))

(use-package cc-vars
  :config
  (mapc (lambda (m) (setq-default c-default-style (+alist-set (car m) (cdr m) c-default-style)))
        '((c-mode . "k&r") (c++-mode . "k&r"))))

(use-package c-ts-mode
  :custom
  (c-ts-mode-indent-style 'k&r))

(use-package hideshow
  ;; Hide/show code blocks, a.k.a. code folding
  :hook ((prog-mode conf-mode nxml-mode) . hs-minor-mode)
  :custom
  (hs-hide-comments-when-hiding-all nil)
  :bind (:map hs-minor-mode-map
              ("C-c f" . #'hs-toggle-hiding)
              ("C-c F" . #'+hs-toggle-all))
  :config
  (defvar-local +hs-toggle-all-show nil)
  (defun +hs-toggle-all ()
    (interactive)
    (if +hs-toggle-all-show (hs-show-all) (hs-hide-all))
    (setq +hs-toggle-all-show (not +hs-toggle-all-show)))

  ;; Add extra modes support
  (unless (assq 't hs-special-modes-alist)
    (cl-callf2 append
        '((vimrc-mode "{{{" "}}}" "\"")
          (ruby-mode "class\\|d\\(?:ef\\|o\\)\\|module\\|[[{]"
           "end\\|[]}]"
           "#\\|=begin"
           ruby-forward-sexp)
          (matlab-mode "if\\|switch\\|case\\|otherwise\\|while\\|for\\|try\\|catch"
           "end"
           nil (lambda (_arg) (matlab-forward-sexp)))
          (nxml-mode "<!--\\|<[^/>]*[^/]>"
           "-->\\|</[^/>]*[^/]>"
           "<!--" sgml-skip-tag-forward nil)
          (latex-mode
           ;; LaTeX-find-matching-end needs to be inside the env
           ("\\\\begin{[a-zA-Z*]+}\\(\\)" 1)
           "\\\\end{[a-zA-Z*]+}"
           "%"
           (lambda (_arg)
             ;; Don't fold whole document, that's useless
             (unless (save-excursion (search-backward "\\begin{document}" (line-beginning-position) t))
              (LaTeX-find-matching-end)))
           nil))
        hs-special-modes-alist '((t)))))

(use-package xref
  :straight t
  :custom
  ;; Use completion in the minibuffer instead of definitions buffer
  (xref-show-definitions-function #'xref-show-definitions-completing-read)
  ;; NOTE: Usually, this shorcut can be bound to moves the window (set by the OS
  ;; window manager), so we need to disable it in the WM for this to work.
  :bind ("M-<down-mouse-1>" . xref-find-references-at-mouse))

(use-package eglot
  :straight t
  :hook (eglot-managed-mode . eglot-inlay-hints-mode)
  :custom
  (eglot-autoshutdown t) ; shutdown after closing the last managed buffer
  (eglot-sync-connect 0) ; async, do not block
  (eglot-extend-to-xref t) ; can be interesting!
  (eglot-report-progress nil) ; disable annoying messages in echo area!
  :config
  (+eglot-register
    '(c++-mode c++-ts-mode c-mode c-ts-mode)
    '("clangd"
      "--background-index"
      "-j=12"
      "--query-driver=/usr/bin/**/clang-*,/bin/clang,/bin/clang++,/usr/bin/gcc,/usr/bin/g++"
      "--clang-tidy"
      ;; "--clang-tidy-checks=*"
      "--all-scopes-completion"
      "--cross-file-rename"
      "--completion-style=detailed"
      "--header-insertion-decorators"
      "--header-insertion=iwyu"
      "--pch-storage=memory")
    "ccls")

  (+eglot-register '(awk-mode awk-ts-mode) "awk-language-server")

  ;; Optimization from Doom Emacs
  ;; NOTE: This setting disable the `eglot-events-buffer' enabling more
  ;; consistent performance on long running Emacs instance. Default is 2000000
  ;; lines. After each new event the whole buffer is pretty printed which causes
  ;; steady performance decrease over time. CPU is spent on pretty priting and
  ;; Emacs GC is put under high pressure.
  (unless minemacs-debug-p
    (cl-callf plist-put eglot-events-buffer-config :size 0)))

(use-package eldoc
  :straight t
  :custom
  (eldoc-documentation-strategy #'eldoc-documentation-compose))

(use-package sqlite-mode
  :when (and (>= emacs-major-version 29) (+emacs-features-p 'sqlite3))
  :config
  (+nvmap! :keymaps 'sqlite-mode-map
    "X" #'sqlite-mode-delete
    "d" #'sqlite-mode-list-data
    "t" #'sqlite-mode-list-tables
    "c" #'sqlite-mode-list-columns))

(use-package compile
  :commands +toggle-bury-compilation-buffer-if-successful
  ;; Enable ANSI colors in compilation buffer
  :hook (compilation-filter . ansi-color-compilation-filter)
  :hook (shell-mode . compilation-shell-minor-mode)
  :custom
  (compilation-scroll-output t) ; Keep scrolling the compilation buffer, `first-error' can be interesting
  (compilation-always-kill t) ; Always kill current compilation process before starting a new one
  (compilation-skip-visited t) ; Skip visited messages on compilation motion commands
  (compilation-window-height 12) ; Keep it readable
  :init
  (defcustom +compilation-auto-bury-msg-level "warning"
    "Level of messages to consider OK to auto-bury the compilation buffer."
    :group 'minemacs-prog
    :type '(choice (const "warning") (const "error") string))
  (defcustom +compilation-auto-bury-delay 3.0
    "The delay in seconds after which the compilation buffer is buried."
    :group 'minemacs-prog
    :type 'number)
  :config
  ;; Integration of `compile' with `savehist'
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'compile-history))

  ;; Auto-close the compilation buffer if succeeded without warnings.
  ;; Adapted from: stackoverflow.com/q/11043004/3058915
  (defun +compilation--bury-if-successful-h (buf str)
    "Bury the compilation buffer if it succeeds without warnings."
    (when (and
           (string-match "compilation" (buffer-name buf))
           (string-match "finished" str)
           (not (with-current-buffer buf
                  (save-excursion
                    (goto-char (point-min))
                    (search-forward +compilation-auto-bury-msg-level nil t)))))
      (run-at-time
       +compilation-auto-bury-delay nil
       (lambda (b)
         (with-selected-window (get-buffer-window b)
           (kill-buffer-and-window))
         (unless (current-message)
           (message "Compilation finished without warnings.")))
       buf)))

  (defun +toggle-bury-compilation-buffer-if-successful ()
    "Toggle auto-burying the successful compilation buffer."
    (interactive)
    (if (memq '+compilation--bury-if-successful-h compilation-finish-functions)
        (progn
          (message "Disabled burying compilation buffer.")
          (remove-hook 'compilation-finish-functions #'+compilation--bury-if-successful-h))
      (message "Enabled burying compilation buffer.")
      (add-hook 'compilation-finish-functions #'+compilation--bury-if-successful-h))))

(use-package vhdl-mode
  :config
  ;; Setup vhdl_ls from rust_hdl (AUR: rust_hdl-git)
  (+eglot-register '(vhdl-mode vhdl-ts-mode) "vhdl_ls"))

(use-package verilog-mode
  :config
  ;; Setup Verilog/SystemVerilog LSP servers
  (+eglot-register '(verilog-mode verilog-ts-mode) "svls" "verible-verilog-ls" "svlangserver"))

(use-package nxml-mode
  :mode "\\.xmpi\\'"
  :config
  (+eglot-register '(nxml-mode xml-mode) "lemminx"))

(use-package elisp-mode
  :after minemacs-first-elisp-file ; prevent elisp-mode from being loaded too early
  :custom-face ; better the default cyan color!
  (elisp-shorthand-font-lock-face ((t :inherit font-lock-keyword-face :foreground "red")))
  :init
  (+setq-hook! emacs-lisp-mode tab-width 8)) ; to view built-in packages correctly

(use-package edebug
  :after elisp-mode
  :custom
  (edebug-inhibit-emacs-lisp-mode-bindings t))

(use-package scheme
  :custom
  (scheme-program-name "guile"))

(use-package gdb-mi
  :custom
  (gdb-show-main t) ; display source file containing main routine at startup
  (gdb-many-windows t) ; start in gdb-many-windows mode
  (gdb-debug-log-max 1024) ; default 128
  (gdb-restore-window-configuration-after-quit t)
  (gdb-thread-buffer-verbose-names nil)
  (gdb-window-configuration-directory (+directory-ensure minemacs-local-dir "gdb/"))
  (gdb-max-source-window-count 1) ; IDEA: maybe increase it!
  (gdb-display-io-nopopup nil)) ; IDEA: maybe change it!

(use-package gud
  :config
  ;; Add an overlay for the current line (mimics dap-mode)
  (defvar +gud-overlay
    (let* ((overlay (make-overlay (point-min) (point-min))))
      (overlay-put overlay 'face 'highlight)
      overlay)
    "Overlay variable for GUD highlighting.")

  (advice-add
   'gud-display-line :after
   (satch-defun +gud--display-overlay:after-a (true-file _line)
     (let* ((overlay +gud-overlay)
            (buffer (gud-find-file true-file)))
       (with-current-buffer buffer
         (move-overlay overlay (line-beginning-position) (line-end-position) (current-buffer))))))

  (add-hook
   'kill-buffer-hook
   (satch-defun +gud--delete-overlay-h ()
     (when (derived-mode-p 'gud-mode)
       (delete-overlay +gud-overlay)))))

(use-package python
  :init
  (defcustom +python-enable-pyenv nil
    "Enable integration for pyenv.
This variable should be set early, either in \"early-config.el\" or \"init-tweaks.el\"."
    :group 'minemacs-utils
    :type 'boolean)
  (when (and +python-enable-pyenv (executable-find "pyenv") (file-directory-p "~/.pyenv/shims/"))
    (setenv "WORKON_HOME" "~/.pyenv/versions")
    (setenv "PIPENV_PYTHON" "~/.pyenv/shims/python")
    (setenv "VIRTUALENVWRAPPER_PYTHON" "~/.pyenv/shims/python")
    (setenv "VIRTUALENVWRAPPER_VIRTUALENV" "~/.pyenv/shims/python")))

(use-package org
  :straight (:type built-in)
  :preface
  ;; Set to nil so we can detect user changes (in config.el)
  (setq org-directory nil)
  :custom
  (org-auto-align-tags nil)
  (org-clock-persist-file (concat minemacs-cache-dir "org/clock-persist.el"))
  (org-cycle-hide-block-startup t)
  (org-edit-src-auto-save-idle-delay auto-save-timeout) ; use the defaults
  (org-edit-src-content-indentation 0) ; do not indent the content of src blocks
  (org-edit-src-turn-on-auto-save t) ; auto-save org-edit-src
  (org-ellipsis " ↩")
  (org-export-async-init-file (expand-file-name (concat minemacs-modules-dir "extras/me-org-export-async-init.el")))
  (org-export-in-background t) ; run export processes in external emacs process
  (org-export-with-broken-links 'mark) ; Do not rise error on broken links, but mark them in the output file
  (org-export-with-smart-quotes t) ; convert "this" to « this »
  (org-export-with-sub-superscripts '{}) ; Only explicit _{} ^{} are interpreted as sub/superscripts
  (org-fold-catch-invisible-edits 'smart) ; try not to accidentally do weird stuff in invisible regions
  (org-fontify-quote-and-verse-blocks t)
  (org-hide-emphasis-markers t)
  (org-highlight-latex-and-related '(native latex script entities))
  (org-id-locations-file (concat minemacs-cache-dir "org/id-locations.el"))
  (org-insert-heading-respect-content t)
  (org-list-allow-alphabetical t) ; have a. A. a) A) list bullets
  (org-log-done 'time) ; having the time an item is done sounds convenient
  (org-persist-directory (+directory-ensure minemacs-cache-dir "org/persist/"))
  (org-pretty-entities t)
  (org-pretty-entities-include-sub-superscripts t)
  (org-preview-latex-image-directory (+directory-ensure minemacs-cache-dir "org/preview/latex-image/"))
  (org-publish-timestamp-directory (+directory-ensure minemacs-cache-dir "org/publish/timestamps/"))
  (org-return-follows-link t) ; RET follows link (a key bind has to be defined for Evil, (see `me-evil'))
  (org-special-ctrl-a/e t)
  (org-startup-indented nil)
  (org-tags-column 0)
  (org-use-property-inheritance t) ; it's convenient to have properties inherited
  (org-use-sub-superscripts '{}) ; Do the same when rendering the Org buffer
  :config
  (setq org-export-async-debug minemacs-debug-p) ;; Can be useful!

  ;; TEMP: This solve the "Invalid face reference: org-indent [X times]" problem.
  (require 'org-indent)

  ;; Dynamically change font size for Org heading levels, starting from
  ;; `+org-level-base-size', and shrinking by a factor of 0.9 at each level.
  (defvar +org-level-base-size 1.3)

  (dotimes (level 8)
    (let ((size (max 1.0 (* +org-level-base-size (expt 0.9 level)))))
      (set-face-attribute
       (intern (format "org-level-%d" (1+ level))) nil
       :weight 'bold
       :height size)))

  (org-babel-do-load-languages
   'org-babel-load-languages
   (cl-loop
    for lang in '(C R js dot awk sed sql org shell ditaa latex julia sqlite octave
                  maxima eshell scheme python fortran gnuplot plantuml makefile)
    collect (cons lang t)))

  (with-eval-after-load 'org-src
    (+alist-set "dot" 'graphviz-dot org-src-lang-modes))

  (with-eval-after-load 'plantuml-mode
    (setq org-plantuml-jar-path plantuml-jar-path
          org-plantuml-exec-mode plantuml-default-exec-mode
          org-plantuml-executable-path plantuml-executable-path))

  ;; From Doom Emacs
  (with-no-warnings
    (custom-declare-face '+org-todo-active  '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
    (custom-declare-face '+org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
    (custom-declare-face '+org-todo-onhold  '((t (:inherit (bold warning org-todo)))) "")
    (custom-declare-face '+org-todo-cancel  '((t (:inherit (bold error org-todo)))) ""))

  (setq org-todo-keywords
        '((sequence
           "TODO(t)"  ; A task that needs doing & is ready to do
           "PROJ(p)"  ; A project, which usually contains other tasks
           "LOOP(r)"  ; A recurring task
           "STRT(s)"  ; A task that is in progress
           "WAIT(w)"  ; Something external is holding up this task
           "HOLD(h)"  ; This task is paused/on hold because of me
           "IDEA(i)"  ; An unconfirmed and unapproved task or notion
           "|"
           "DONE(d)"  ; Task successfully completed
           "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
          (sequence
           "[ ](T)"   ; A task that needs doing
           "[-](S)"   ; Task is in progress
           "[?](W)"   ; Task is being held up or paused
           "|"
           "[X](D)")  ; Task was completed
          (sequence
           "|"
           "OKAY(o)"
           "YES(y)"
           "NO(n)"))
        org-todo-keyword-faces
        '(("[-]"  . +org-todo-active)
          ("STRT" . +org-todo-active)
          ("[?]"  . +org-todo-onhold)
          ("WAIT" . +org-todo-onhold)
          ("HOLD" . +org-todo-onhold)
          ("PROJ" . +org-todo-project)
          ("NO"   . +org-todo-cancel)
          ("KILL" . +org-todo-cancel))))

(use-package org-agenda
  :custom
  (org-agenda-tags-column 0))

(use-package ox-latex
  :after ox
  :custom
  (org-latex-src-block-backend 'engraved)
  (org-latex-prefer-user-labels t)
  (org-latex-tables-booktabs t)
  ;; Default `minted` options, can be overwritten in file/dir locals
  (org-latex-minted-options
   '(("frame"         "lines")
     ("fontsize"      "\\footnotesize")
     ("tabsize"       "2")
     ("breaklines"    "true")
     ("breakanywhere" "true") ;; break anywhere, no just on spaces
     ("style"         "default")
     ("bgcolor"       "GhostWhite")
     ("linenos"       "true")))
  :config
  ;; (setq org-latex-line-break-safe "\\\\")
  ;; Add this to your config to be able to export with minted:
  ;; (with-eval-after-load 'ox-latex
  ;;   (add-to-list 'org-latex-packages-alist '("" "minted"))
  ;;   (add-to-list 'org-latex-packages-alist '("svgnames" "xcolor"))
  ;;   (setq org-latex-src-block-backend 'minted
  ;;         org-latex-pdf-process '("latexmk -f -pdf -%latex -shell-escape -interaction=nonstopmode -output-directory=%o %f")))

  ;; Map some org-mode blocks' languages to lexers supported by minted
  ;; you can see supported lexers by running this command in a terminal:
  ;; 'pygmentize -L lexers'
  (dolist (pair '((ipython    "python")
                  (jupyter    "python")
                  (scheme     "scheme")
                  (lisp-data  "lisp")
                  (conf-unix  "unixconfig")
                  (conf-space "unixconfig")
                  (authinfo   "unixconfig")
                  (gdb-script "unixconfig")
                  (conf-toml  "yaml")
                  (conf       "ini")
                  (gitconfig  "ini")
                  (systemd    "ini")))
    (unless (member pair org-latex-minted-langs)
      (add-to-list 'org-latex-minted-langs pair)))

  (cond
   ((executable-find "latexmk")
    (setq
     org-latex-pdf-process
     '("latexmk -c -bibtex-cond1 %f" ; ensure cleaning ".bbl" files
       "latexmk -f -pdf -%latex -shell-escape -interaction=nonstopmode -output-directory=%o %f")))
   ;; NOTE: Tectonic might have some issues with some documents (sagej + natbib)
   ((executable-find "tectonic")
    (setq
     org-latex-pdf-process
     '("tectonic -X compile --outdir=%o -Z shell-escape -Z continue-on-errors %f")))))

(use-package ox
  :config
  (mapc #'require '(ox-odt ox-beamer ox-koma-letter)))

(use-package oc
  :after org
  :demand
  :custom
  (org-cite-export-processors '((latex biblatex) (t csl)))
  (org-support-shift-select t)
  :config
  (mapc #'require '(oc-csl oc-natbib oc-biblatex)))

(use-package electric
  :config
  ;; Electric indent on delete and enter
  (setq-default electric-indent-chars '(?\n ?\^?))

  (defvar-local +electric-indent-words '()
    "The list of electric words. Typing these will trigger reindentation of the
current line.")

  ;; Electric indent at Bash/Sh keywords, extracted from the grammar
  (+setq-hook! (sh-mode bash-ts-mode)
    +electric-indent-words
    (delete-dups (apply #'append (mapcar (lambda (e) (list (car e) (cdr e))) (cdar sh-smie-sh-grammar)))))

  ;; From Doom Emacs
  (add-hook
   'electric-indent-functions
   (satch-defun +electric-indent-char-fn (_c)
     (when (and (eolp) +electric-indent-words)
       (save-excursion
         (backward-word)
         (looking-at-p (concat "\\<" (regexp-opt +electric-indent-words))))))))

(use-package ediff
  :hook (ediff-before-setup . +ediff--save-window-config-h)
  :custom
  ;; Split horizontally
  (ediff-split-window-function #'split-window-horizontally)
  ;; Setup all windows in one frame
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  :config
  (defvar +ediff--saved-window-config nil)

  ;; Save the current window configuration (hooked to `ediff-before-setup-hook')
  (defun +ediff--save-window-config-h ()
    (setq +ediff--saved-window-config (current-window-configuration)))

  ;; Restore the saved window configuration on quit or suspend
  (+add-hook! (ediff-quit ediff-suspend) :depth 101
    (satch-defun +ediff--restore-window-config-h ()
      (when (window-configuration-p +ediff--saved-window-config)
        (set-window-configuration +ediff--saved-window-config)))))

(use-package smerge-mode
  :commands +smerge-hydra/body
  :config
  (defun +smerge-first ()
    "Got to the first occurrence."
    (interactive)
    (goto-char (point-min))
    (smerge-next))

  (defun +smerge-last ()
    "Got to the last occurrence."
    (interactive)
    (goto-char (point-max))
    (smerge-prev))

  (with-eval-after-load 'hydra
    (defhydra +smerge-hydra (:hint nil
                                   :pre (if (not smerge-mode) (smerge-mode 1))
                                   ;; Disable `smerge-mode' when quitting hydra if
                                   ;; no merge conflicts remain.
                                   :post (smerge-auto-leave))
      "
                                                         [smerge]
  Movement   Keep           Diff              Other         │
  ╭─────────────────────────────────────────────────────────╯
  │  ^_g_^       [_b_] base       [_<_] upper/base    [_C_] Combine
  │  ^_C-p_^     [_u_] upper      [_=_] upper/lower   [_r_] resolve
  │  ^_p_ ↑^     [_l_] lower      [_>_] base/lower    [_R_] remove
  │  ^_n_ ↓^     [_a_] all        [_H_] highlight     [_n_] next in project
  │  ^_C-n_^     [_RET_] current  [_E_] ediff
  │  ^_G_^                                                 [_q_] quit
  ╰─────────────────────────────────────────────────────╯
"
      ("g" +smerge-first)
      ("G" +smerge-last)
      ("C-n" smerge-next)
      ("C-p" smerge-prev)
      ("n" next-line)
      ("p" previous-line)
      ("b" smerge-keep-base)
      ("u" smerge-keep-upper)
      ("l" smerge-keep-lower)
      ("a" smerge-keep-all)
      ("RET" smerge-keep-current)
      ("\C-m" smerge-keep-current)
      ("<" smerge-diff-base-upper)
      ("=" smerge-diff-upper-lower)
      (">" smerge-diff-base-lower)
      ("H" smerge-refine)
      ("E" smerge-ediff)
      ("C" smerge-combine-with-next)
      ("r" smerge-resolve)
      ("R" smerge-kill-current)
      ;; Often after calling `smerge-vc-next-conflict', the cursor will land at
      ;; the bottom of the window
      ("n" (progn (smerge-vc-next-conflict) (recenter-top-bottom (/ (window-height) 8))))
      ("q" nil :color blue))))

(use-package octave
  :mode ("\\.m\\'" . octave-maybe-mode)
  :config
  (defun +octave-eval-last-sexp ()
    "Evaluate Octave sexp before point and print value into current buffer."
    (interactive)
    (inferior-octave t)
    (let ((print-escape-newlines nil)
          (opoint (point)))
      (prin1
       (save-excursion
         (forward-sexp -1)
         (inferior-octave-send-list-and-digest
          (list (concat (buffer-substring-no-properties (point) opoint) "\n")))
         (mapconcat 'identity inferior-octave-output-list "\n"))))))

(use-package abbrev
  :custom
  (abbrev-file-name (concat minemacs-local-dir "abbrev.el")))

(use-package bookmark
  :custom
  (bookmark-default-file (concat minemacs-local-dir "bookmark.el"))
  ;; Save the bookmarks every time a bookmark is made
  (bookmark-save-flag 1)
  :config
  (push bookmark-default-file +first-file-hook-ignore-list))

(use-package calc
  :custom
  (calc-settings-file (concat minemacs-local-dir "calc-settings.el")))

(use-package desktop
  :custom
  ;; File name to use when saving desktop
  (desktop-base-file-name "emacs-session.el")
  ;; File name to use as a lock
  (desktop-base-lock-name (concat desktop-base-file-name ".lock"))
  ;; Load only 5 buffers immediately, the remaining buffers will be loaded lazily
  (desktop-restore-eager 5)
  ;; Avoid writing contents unchanged between auto-saves
  (desktop-file-checksum t)
  ;; Save buffer status
  (desktop-save-buffer t))

(use-package recentf
  :custom
  (recentf-save-file (concat minemacs-local-dir "recentf-save.el"))
  ;; Increase the maximum number of saved items
  (recentf-max-saved-items 200)
  ;; Ignore case when searching recentf files
  (recentf-case-fold-search t)
  ;; Exclude some files from being remembered by recentf
  (recentf-exclude
   `(file-remote-p
     ,(rx (or "/elfeed-db/" "/eln-cache/" "/cache/" "/.maildir/" "/.cache/"))
     ,(rx bol "/tmp/")))
  :bind (("C-c c r" . recentf-open-files))
  :init
  ;; Enable `recentf-mode' to remember recent files
  (+shutup! (recentf-mode 1)))

(use-package url
  :custom
  (url-cache-directory (+directory-ensure minemacs-cache-dir "url/"))
  (url-configuration-directory (+directory-ensure minemacs-local-dir "url/"))
  (url-cookie-file (concat minemacs-local-dir "url/cookie.el"))
  (url-history-file (concat minemacs-local-dir "url/history.el")))

(use-package webjump
  :custom
  (webjump-sites
   '(("Emacs Wiki"    . [simple-query "www.emacswiki.org" "www.emacswiki.org/cgi-bin/wiki/" ""])
     ("DuckDuckGo"    . [simple-query "duckduckgo.com" "duckduckgo.com/?q=" ""])
     ("Qwant"         . [simple-query "www.qwant.com" "www.qwant.com/?q=" ""])
     ("Ecosia"        . [simple-query "www.ecosia.org" "www.ecosia.org/search?q=" ""])
     ("Brave"         . [simple-query "search.brave.com" "search.brave.com/search?q=" ""])
     ("Bing"          . [simple-query "www.bing.com" "www.bing.com/search?q=" ""])
     ("Yahoo"         . [simple-query "www.yahoo.com" "search.yahoo.com/search?p=" ""])
     ("Google"        . [simple-query "www.google.com" "www.google.com/search?q=" ""])
     ("Google Maps"   . [simple-query "www.google.com" "www.google.com/maps?q=" ""])
     ("Google Images" . [simple-query "www.google.com" "www.google.com/images?q=" ""])
     ("Google Groups" . [simple-query "groups.google.com" "groups.google.com/groups?q=" ""])
     ("StackOverflow" . [simple-query "stackoverflow.com" "stackoverflow.com/search?q=" ""])
     ("GitHub Repo"   . [simple-query "github.com" "github.com/search?type=repositories&q=" ""])
     ("GitHub Code"   . [simple-query "github.com" "github.com/search?type=code&q=" ""])
     ("WolframAlpha"  . [simple-query "wolframalpha.com" "wolframalpha.com/input/?i=" ""])
     ("MDN"           . [simple-query "developer.mozilla.org" "developer.mozilla.org/search?q=" ""])
     ("Youtube"       . [simple-query "www.youtube.com" "www.youtube.com/results?search_query=" ""])
     ("Reddit"        . [simple-query "www.reddit.com" "www.reddit.com/search/?q=" ""])
     ("Wikipedia"     . [simple-query "wikipedia.org" "wikipedia.org/wiki/" ""]))))

(use-package time-stamp
  :hook (before-save . time-stamp) ; Update time stamp (if available) before saving a file.
  :custom
  ;; Do enable time-stamps
  (time-stamp-active t)
  ;; Check the first 12 buffer lines for Time-stamp: <>
  (time-stamp-line-limit 12)
  ;; Timestamp format
  (time-stamp-format "%04Y-%02m-%02d %02H:%02M:%02S"))

(use-package whitespace
  :custom
  (whitespace-action '(cleanup auto-cleanup))) ; Default behavior for `whitespace-cleanup'

(use-package autorevert
  ;; Auto load files changed on disk
  :hook (minemacs-first-file . global-auto-revert-mode)
  :custom
  ;; Revert non-file buffers like dired
  (global-auto-revert-non-file-buffers t))

(use-package savehist
  :hook (minemacs-lazy . savehist-mode)
  :custom
  (savehist-file (concat minemacs-local-dir "savehist.el")))

(use-package saveplace
  ;; Save place in files
  :hook (minemacs-first-file . save-place-mode)
  :custom
  (save-place-file (concat minemacs-local-dir "save-place.el")))

(use-package term
  :config
  ;; Kill `term' buffer on exit (reproduce a similar behavior to `shell's
  ;; `shell-kill-buffer-on-exit').
  (advice-add
   'term-sentinel :around
   (satch-defun +term--kill-after-exit:around-a (orig-fn proc msg)
     (if (memq (process-status proc) '(signal exit))
         (let ((buffer (process-buffer proc)))
           (apply orig-fn (list proc msg))
           (kill-buffer buffer))
       (apply orig-fn (list proc msg))))))

(use-package executable
  ;; Make scripts (files starting with shebang "#!") executable when saved
  :hook (after-save . executable-make-buffer-file-executable-if-script-p))

(use-package display-line-numbers
  ;; Show line numbers
  :hook ((prog-mode conf-mode text-mode) . display-line-numbers-mode)
  :custom
  ;; Width for line numbers
  (display-line-numbers-width 4)
  ;; Display absolute line numbers in narrowed regions
  (display-line-numbers-widen t))

(use-package pixel-scroll
  :hook (minemacs-lazy . +pixel-scroll-mode)
  :custom
  ;; Better scrolling on Emacs29+, specially on a touchpad
  (pixel-scroll-precision-use-momentum t)
  :config
  (defun +pixel-scroll-mode ()
    ;; Scroll pixel by pixel, in Emacs29+ there is a more pricise mode way to scroll
    (if (>= emacs-major-version 29)
        (pixel-scroll-precision-mode 1)
      (pixel-scroll-mode 1))))

(use-package mouse
  ;; Enable context menu on mouse right click
  :hook (minemacs-lazy . context-menu-mode)
  :custom
  ;; Enable Drag-and-Drop of regions
  (mouse-drag-and-drop-region t)
  ;; Enable Drag-and-Drop of regions from Emacs to external programs
  (mouse-drag-and-drop-region-cross-program t))

(use-package mwheel
  :custom
  ;; Make mouse scroll a little faster
  (mouse-wheel-scroll-amount '(2 ((shift) . hscroll) ((meta) . nil) ((control meta) . global-text-scale) ((control) . text-scale)))
  ;; Make mouse scroll a little faster horizontally
  (mouse-wheel-scroll-amount-horizontal 2))

(use-package gnus
  :custom
  (gnus-dribble-directory (+directory-ensure minemacs-local-dir "gnus/dribble/"))
  (gnus-init-file (concat minemacs-config-dir "gnus/init.el"))
  (gnus-startup-file (concat minemacs-config-dir "gnus/newsrc")))

(use-package image-dired
  :custom
  (image-dired-dir (+directory-ensure minemacs-local-dir "image-dired/"))
  (image-dired-tags-db-file (concat minemacs-local-dir "image-dired/tags-db.el"))
  (image-dired-temp-rotate-image-file (concat minemacs-cache-dir "image-dired/temp-rotate-image")))

(use-package time
  ;; Display time in mode-line
  :hook (minemacs-lazy . display-time-mode)
  :custom
  ;; Enable time in the mode-line
  (display-time-string-forms '((propertize (concat 24-hours ":" minutes)))))

(use-package frame
  ;; Display divider between windows
  :hook (minemacs-lazy . window-divider-mode)
  :custom
  ;; Set line width for the divider in `window-divider-mode' to 2px
  (window-divider-default-bottom-width 2)
  (window-divider-default-right-width 2))

(use-package repeat
  ;; Enable repeat mode, "C-x o then C-x o" becomes "C-x o o"
  :hook (minemacs-lazy . repeat-mode))

(use-package server
  :autoload server-running-p
  :hook ((server-after-make-frame minemacs-after-startup) . +scratch-replace-with-persistent-scratch)
  :init
  ;; When we start in a non-daemon Emacs, we start a server when Emacs is idle.
  (unless (daemonp)
    (+lazy!
     (unless (server-running-p)
       (let ((inhibit-message t))
         (server-start nil t)
         (+info! "Started Emacs daemon in background."))))))

(use-package speedbar ; config from Crafted Emacs
  :custom
  (speedbar-update-flag t) ; Auto-update when the attached frame changes directory
  (speedbar-use-images nil) ; Disable icon images, instead use text
  (speedbar-frame-parameters ; Customize Speedbar Frame
   '((name . "speedbar")
     (title . "speedbar")
     (minibuffer . nil)
     (border-width . 2)
     (menu-bar-lines . 0)
     (tool-bar-lines . 0)
     (unsplittable . t)
     (left-fringe . 10)))
  :config
  ;; File Extensions
  (speedbar-add-supported-extension
   '(;; Classic Lisp Languages
     ".cl" ".el" ".scm" ".lisp"
     ;; Lua/Fennel (Lisp that transpiles to lua)
     ".lua" ".fnl" ".fennel"
     ;; JVM languages (Java, Kotlin, Clojure)
     ".java" ".kt" ".mvn" ".gradle" ".properties" ".clj"
     ;; C/C++
     ".c" ".cpp" ".cc" ".h" ".hh" ".hpp"
     ;; Shell scripts
     ".sh" ".bash"
     ;; Web Languages and Markup/Styling
     ".php" ".js" ".ts" ".html" ".htm" ".css" ".less" ".scss" ".sass"
     ;; Makefile
     "makefile" "MAKEFILE" "Makefile"
     ;; Data formats
     ".json" ".yaml" ".toml"
     ;; Notes and Markup
     ".md" ".markdown" ".org" ".txt" "README")))

(use-package simple
  :init
  ;; Never mix, use only spaces
  (setq-default indent-tabs-mode nil)
  ;; Show line number in mode-line
  :hook (minemacs-lazy . line-number-mode)
  ;; Show column numbers (a.k.a. cursor position) in the mode-line
  :hook (minemacs-lazy . column-number-mode)
  ;; Display buffer size on mode line
  :hook (minemacs-lazy . size-indication-mode)
  ;; Wrap long lines
  :hook ((prog-mode conf-mode text-mode) . visual-line-mode)
  :custom
  ;; Filter duplicate entries in kill ring
  (kill-do-not-save-duplicates t)
  ;; Save existing clipboard text into the kill ring before replacing it.
  (save-interprogram-paste-before-kill t))

(use-package help
  :custom
  ;; Select help window for faster quit!
  (help-window-select t))

(use-package winner
  ;; Window layout undo/redo (`winner-undo' / `winner-redo')
  :hook (minemacs-lazy . winner-mode))

(use-package delsel
  ;; Replace selection after start typing
  :hook (minemacs-lazy . delete-selection-mode))

(use-package mb-depth
  ;; Show recursion depth in minibuffer (see `enable-recursive-minibuffers')
  :hook (minemacs-lazy . minibuffer-depth-indicate-mode))

(use-package subword
  ;; Global SubWord mode
  :hook (minemacs-lazy . global-subword-mode))

(use-package so-long
  ;; Better handling for files with so long lines
  :hook (minemacs-after-startup . global-so-long-mode))

(use-package icomplete
  ;; Fallback the new `fido-vertical-mode' Emacs28+ builtin completion mode if
  ;; the `me-completion' (which contains `vertico-mode' configuration) core
  ;; module is not enabled.
  :when (+package-disabled-p 'vertico 'me-completion)
  :hook (minemacs-lazy . fido-vertical-mode))

(use-package battery
  :hook (minemacs-lazy . +display-battery-mode-maybe)
  :init
  ;; Show the battery status (if available) in the mode-line
  (defun +display-battery-mode-maybe ()
    (+shutup!
     (when-let* ((battery-str (battery))
                 (_ (not (or (equal "Battery status not available" battery-str)
                             (string-match-p "unknown" battery-str)
                             (string-match-p "N/A" battery-str)))))
       (display-battery-mode 1)))))

(use-package windmove
  :after minemacs-lazy
  :demand
  :config
  ;; Navigate windows using Shift+Direction
  (windmove-default-keybindings 'shift)
  (defvar-keymap +windmove-keys
    :repeat t ; Make it work with `repeat-mode'
    "k" #'windmove-up
    "j" #'windmove-down
    "h" #'windmove-left
    "l" #'windmove-right)
  (keymap-global-set "C-c w" +windmove-keys))

(use-package pulse
  :init
  ;; Add visual pulse when changing focus, like beacon but built-in
  ;; From: https://karthinks.com/software/batteries-included-with-emacs/
  (dolist (command '(scroll-up-command scroll-down-command recenter-top-bottom other-window))
    (advice-add
     command :after
     (satch-defun +pulse--line:after-a (&rest _)
       "Pulse the current line."
       (pulse-momentary-highlight-one-line (point))))))

(use-package isearch
  :bind (:map
         isearch-mode-map
         ("<up>" . isearch-ring-retreat)
         ("<down>" . isearch-ring-advance)))

(use-package yaml-ts-mode
  :when (+emacs-features-p 'tree-sitter)
  :mode (rx (any ?. ?_) (or "clang-format" "clang-tidy") eol))


(provide 'me-builtin)

;;; me-builtin.el ends here
