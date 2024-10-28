;; me-evil.el --- Emacs as Vim! -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(defcustom minemacs-leader-key "SPC"
  "MinEmacs leader key."
  :group 'minemacs-keybinding
  :type 'string)

(defcustom minemacs-localleader-key "SPC m"
  "MinEmacs local leader (a.k.a. mode specific) key sequence."
  :group 'minemacs-keybinding
  :type 'string)

(defcustom minemacs-global-leader-prefix "C-SPC"
  "MinEmacs general leader key."
  :group 'minemacs-keybinding
  :type 'string)

(defcustom minemacs-global-mode-prefix "C-SPC m"
  "MinEmacs general local leader (a.k.a. mode specific) key sequence."
  :group 'minemacs-keybinding
  :type 'string)

;;; Keybinding macros
;;; =================

;; TEMP: These macros are specific to `evil' and `general'. MinEmacs moved since
;; v7.0.0 to a more classic (non Evil-based) keybindings.
;; PERF+HACK: At some point, MinEmacs startup become too slow, specially when
;; initializing `general' and `evil'. After trying several configurations, I
;; figured out that deferring `general' solves the issue. However, deferring
;; `general' means that we cannot define the keybindings when loading other
;; packages, i.e. before `general' gets loaded and the MinEmacs definers (i.e.
;; `+minemacs--internal-map!', `+minemacs--internal-map-local!', ...) are made
;; available. We overcome this by defining these macros to define the
;; keybindings by wrapping the actual definition in a `with-eval-after-load'
;; block to be evaluated only after `general' gets loaded and configured and the
;; definers are ready (See `me-keybindings').
(defmacro +map! (&rest args)
  "A wrapper around `+minemacs--internal-map!'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  (let (pkg mod)
    (when (eq (car args) :package)
      (setq pkg (cadr args)
            args (cddr args))
      (when (eq (car args) :module)
        (setq mod (cadr args)
              args (cddr args))))
    `(unless ,(when pkg (append (list '+package-disabled-p (list 'quote pkg)) (when mod (list (list 'quote mod)))))
      (with-eval-after-load 'me-general-ready
       (+minemacs--internal-map! ,@args)))))

(defmacro +map-local! (&rest args)
  "A wrapper around `+minemacs--internal-map-local!'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  (let (pkg mod)
    (when (eq (car args) :package)
      (setq pkg (cadr args)
            args (cddr args))
      (when (eq (car args) :module)
        (setq mod (cadr args)
              args (cddr args))))
    `(unless ,(when pkg (append (list '+package-disabled-p (list 'quote pkg)) (when mod (list (list 'quote mod)))))
      (with-eval-after-load 'me-general-ready
       (+minemacs--internal-map-local! ,@args)))))

;; Wrappers around `general's VIM like definers, needs `general-evil-setup' to
;; be executed (See `me-keybindings')
(defmacro +nmap! (&rest args)
  "A wrapper around `general-nmap'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 'me-general-ready
    (general-nmap ,@args)))

(defmacro +vmap! (&rest args)
  "A wrapper around `general-vmap'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 'me-general-ready
    (general-vmap ,@args)))

(defmacro +mmap! (&rest args)
  "A wrapper around `general-mmap'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 'me-general-ready
    (general-mmap ,@args)))

(defmacro +imap! (&rest args)
  "A wrapper around `general-imap'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 'me-general-ready
    (general-imap ,@args)))

(defmacro +emap! (&rest args)
  "A wrapper around `general-emap'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 'me-general-ready
    (general-emap ,@args)))

(defmacro +omap! (&rest args)
  "A wrapper around `general-omap'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 'me-general-ready
    (general-omap ,@args)))

(defmacro +rmap! (&rest args)
  "A wrapper around `general-rmap'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 'me-general-ready
    (general-rmap ,@args)))

(defmacro +iemap! (&rest args)
  "A wrapper around `general-iemap'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 'me-general-ready
    (general-iemap ,@args)))

(defmacro +nvmap! (&rest args)
  "A wrapper around `general-nvmap'.
It is deferred until `general' gets loaded and configured."
  (declare (indent defun))
  `(with-eval-after-load 'me-general-ready
    (general-nvmap ,@args)))



(use-package evil
  :straight t
  :hook (minemacs-lazy . evil-mode)
  :preface
  ;; Needed by `evil-collection'
  (setq evil-want-keybinding nil
        evil-want-integration t)
  :custom
  (evil-want-C-i-jump nil)
  (evil-want-fine-undo t)
  (evil-want-Y-yank-to-eol t)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-kill-on-visual-paste nil)
  (evil-undo-system 'undo-redo)
  ;; BUG: setting this to t triggers errors on pressing . to repeat command
  (evil-respect-visual-line-mode nil)
  (evil-ex-interactive-search-highlight 'selected-window)
  :config
  (+map!
    ;; buffer
    "bN" '(evil-buffer-new :wk "New buffer")
    ;; window
    "ww" '(evil-window-next :wk "Next")
    "wW" '(evil-window-prev :wk "Previous")
    "ws" '(evil-window-split :wk "Split")
    "wv" '(evil-window-vsplit :wk "Vertical split")
    "wr" '(evil-window-rotate-downwards :wk "Rotate downwards")
    "wR" '(evil-window-rotate-upwards :wk "Rotate upwards")
    "w+" '(evil-window-increase-width :wk "Increase width")
    "w-" '(evil-window-decrease-width :wk "Decrease width"))

  (+nvmap!
    "gl" #'evil-jump-forward
    "gh" #'evil-jump-backward)

  ;; Use `isearch', it integrates better with other commands like
  ;; `consult-isearch-history'
  (evil-select-search-module 'evil-search-module 'isearch)

  ;; Ask for a buffer when splitting windows
  (with-eval-after-load 'consult
    (dolist (fn '(evil-window-split evil-window-vsplit))
      (advice-add
       fn :after
       (satch-defun +evil--cunsult-buffer-after-window-split:after-a (&rest _)
         (consult-buffer))))))

(use-package evil-collection
  :straight t
  :unless (+package-disabled-p 'evil 'obsolete/me-evil)
  :after evil minemacs-loaded
  :demand
  :config
  (evil-collection-init
   (seq-filter
    (lambda (mode)
      (not (memq mode '(evil-mc ; Default bindings for `evil-mc' are messy
                        elisp-mode)))) ; I don't like "gz" for `ielm', I like "gr" though
    evil-collection-mode-list))

  ;; Use "gr" to find references for elisp mode
  (with-eval-after-load 'elisp-mode
    (when evil-collection-want-find-usages-bindings
      (evil-collection-define-key 'normal 'emacs-lisp-mode-map "gr" 'xref-find-references))))

(use-package evil-snipe
  :straight t
  :unless (+package-disabled-p 'evil 'obsolete/me-evil)
  :hook (evil-mode . evil-snipe-mode)
  :hook (evil-mode . evil-snipe-override-mode)
  :custom
  (evil-snipe-scope 'buffer)
  (evil-snipe-repeat-scope 'buffer)
  (evil-snipe-smart-case t)
  (evil-snipe-auto-scroll t))

(use-package evil-numbers
  :straight t
  :unless (+package-disabled-p 'evil 'obsolete/me-evil)
  :init
  (+nmap!
    "g+" #'evil-numbers/inc-at-pt
    "g=" #'evil-numbers/inc-at-pt
    "g-" #'evil-numbers/dec-at-pt)
  (+vmap!
    "g+" #'evil-numbers/inc-at-pt-incremental
    "g=" #'evil-numbers/inc-at-pt-incremental
    "g-" #'evil-numbers/dec-at-pt-incremental))

(use-package evil-nerd-commenter
  :straight t
  :unless (+package-disabled-p 'evil 'obsolete/me-evil)
  :commands (evilnc-comment-operator evilnc-copy-and-comment-operator)
  :init
  (+nvmap!
    "gc" #'evilnc-comment-operator
    "gC" #'evilnc-copy-and-comment-operator))

(cl-defmacro +evil-conf-for! (package module &optional &key init-form &key config-form)
  (declare (indent 2))
  `(when (and (not (+package-disabled-p ',package ',module)) (memq ',module minemacs-modules))
    ,init-form
    ,(when config-form
      `(with-eval-after-load ',package ,config-form))))

(use-package general
  :straight t
  ;; PERF: Loading `general' early make Emacs very slow on startup.
  :after evil
  :when (memq 'me-keybindings minemacs-modules)
  :demand
  :config
  ;; Advise `define-key' to automatically unbind keys when necessary.
  (general-auto-unbind-keys)
  ;; Set up some basic equivalents (like `general-nmap') with short named
  ;; aliases (like `nmap') for VIM mapping functions.
  (general-evil-setup t)

  ;; Global leader
  (general-create-definer +minemacs--internal-map!
                          ;; The order of states matters, the last is prioritized
                          :states '(insert emacs visual normal)
                          :keymaps 'override
                          :prefix minemacs-leader-key
                          :global-prefix minemacs-global-leader-prefix)

  ;; Local leader
  (general-create-definer +minemacs--internal-map-local!
                          :states '(insert emacs visual normal)
                          :keymaps 'override
                          :prefix minemacs-localleader-key
                          :global-prefix minemacs-global-mode-prefix)

  ;; Define the built-in global keybindings
  (+minemacs--internal-map!
   ;; ====== Top level functions ======
   "SPC"  '(execute-extended-command :wk "M-x")
   ">"    '(switch-to-next-buffer :wk "Next buffer")
   "<"    '(switch-to-prev-buffer :wk "Previous buffer")
   ";"    '(pp-eval-expression :wk "Eval expression")
   ":"    #'project-find-file
   "X"    #'org-capture
   "u"    '(universal-argument :wk "C-u")
   "C"    #'universal-coding-system-argument
   "O"    #'other-window-prefix

   ;; ====== Quit/Session ======
   "q"    '(nil :wk "quit/session")
   "qq"   #'save-buffers-kill-terminal
   "qQ"   #'kill-emacs
   "qS"   #'server-start
   "qR"   #'recover-session
   "qd"   #'desktop-read
   "qD"   #'desktop-lazy-complete
   "qs"   #'desktop-save

   ;; ====== Files ======
   "f"    '(nil :wk "file")
   "fS"   '(write-file :wk "Save as ...")
   "fd"   #'+delete-this-file
   "fD"   #'+delete-this-file-and-buffer
   "fF"   #'+sudo-find-file ; will be overriten with `sudo-edit-find-file'
   "fu"   #'+sudo-this-file ; will be overriten with `sudo-edit'
   "fR"   #'+move-this-file
   "ff"   #'find-file
   "fs"   #'save-buffer
   "ft"   #'recover-this-file
   "fT"   #'recover-file
   "fy"   #'+yank-this-file-name
   "fE"   `(,(+cmdfy! (dired (or minemacs-config-dir minemacs-root-dir)))
            :wk "User config directory")

   ;; ====== Buffers ======
   "b"    '(nil :wk "buffer")
   "bI"   #'ibuffer
   "bu"   #'+sudo-save-buffer
   "bx"   #'bury-buffer
   "bS"   #'save-some-buffers
   "bs"   #'+scratch-open-project-scratch-buffer
   "bM"   #'view-echo-area-messages
   "bA"   #'+kill-some-buffers
   "bk"   `(,(+cmdfy! (kill-buffer (current-buffer)))
            :wk "Kill this buffer")
   "bK"   `(,(+cmdfy! (+kill-buffer-and-its-windows (current-buffer)))
            :wk "Kill this buffer and its windows")
   "br"   '(revert-buffer :wk "Revert")
   "bR"   '(rename-buffer :wk "Rename")
   ;; Lines
   "bl"   '(nil :wk "line")
   "blk"  #'keep-lines ;; Will be overwritten with `consult-keep-lines'
   ;; Bookmarks
   "bm"   '(nil :wk "bookmark")
   "bmm"  #'bookmark-set
   "bmd"  #'bookmark-delete
   ;; Files / Local variables
   "bv"   '(nil :wk "locals")
   "bvv"  '(add-file-local-variable :wk "Add")
   "bvV"  '(delete-file-local-variable :wk "Delete")
   "bvp"  '(add-file-local-variable-prop-line :wk "Add in prop line")
   "bvP"  '(delete-file-local-variable-prop-line :wk "Delete from prop line")
   "bvd"  '(add-dir-local-variable :wk "Add to dir-locals")
   "bvD"  '(delete-dir-local-variable :wk "Delete from dir-locals")
   "bvr"  '(nil :wk "reload dir-locals for...")
   "bvrr" '(+dir-locals-reload-for-this-buffer :wk "This buffer")
   "bvrd" '(+dir-locals-reload-for-all-buffers-in-this-directory :wk "All buffers in this directory")

   ;; ====== Insert ======
   "i"    '(nil :wk "insert")
   "ii"   #'auto-insert
   "iu"   #'insert-char
   "ip"   #'yank-pop ;; Will be overwritten with `consult-yank-pop'
   "ie"   #'emoji-search

   ;; ====== Window ======
   "w"    '(nil :wk "window")
   "wd"   #'delete-window
   "wD"   #'delete-windows-on
   "wo"   #'delete-other-windows
   "wm"   #'maximize-window
   "wu"   #'winner-undo
   "wU"   #'winner-redo

   ;; ====== Applications (Open) ======
   "o"    '(nil :wk "open")
   "o-"   #'dired-jump ;; Will be overwritten if `dirvish' is used
   "oa"   #'org-agenda
   "oe"   #'eshell
   "o="   #'calc

   ;; ====== Search ======
   "s"    '(nil :wk "search")
   "sw"   '+webjump

   ;; ======  Mode specific a.k.a. "local leader" ======
   "m"    '(nil :wk "mode-specific")

   ;; ====== VC ======
   "g"    '(nil :wk "git/vc")

   ;; ====== Workspaces ======
   "TAB"  '(nil :wk "workspace")

   ;; ====== Toggle ======
   "t"    '(nil :wk "toggle")
   "td"   #'toggle-debug-on-error
   "tr"   #'read-only-mode
   "tl"   #'follow-mode
   "tv"   #'visible-mode
   "tf"   #'flymake-mode

   ;; ====== Code ======
   "c"    '(nil :wk "code")
   "cf"   '(nil :wk "format buffer")
   "ce"   '(nil :wk "eglot session")
   "cee"  #'eglot
   "ceA"  #'+eglot-auto-enable

   ;; ====== Debug ======
   "d"    '(nil :wk "debug")
   "dG"   #'gdb

   ;; ====== Notes ======
   "n"    '(nil :wk "notes")

   ;; ====== Help ======
   "h"    '(nil :wk "help")
   "hi"   #'info
   "hg"   #'general-describe-keybindings
   "hs"   #'+screenshot-svg
   "he"   '(nil :wk "elisp/emacs")
   "hes"  #'elisp-index-search
   "hem"  #'info-emacs-manual
   "hei"  #'Info-search
   "hd"   '(nil :wk "describe")
   "hdk"  #'describe-key
   "hdm"  #'describe-keymap
   "hdb"  #'describe-bindings
   "hds"  #'describe-symbol
   "hdv"  #'describe-variable
   "hdc"  #'describe-command
   "hdf"  #'describe-function
   "hdp"  #'describe-package

   ;; ====== Extras ======
   "e"    '(nil :wk "extras")

   ;; ====== Project ======
   "p"    '(nil :wk "project")
   "pw"  #'project-switch-project
   "pc"  #'project-compile
   "pd"  #'project-find-dir
   "pf"  #'project-find-file
   "pk"  #'project-kill-buffers
   "pb"  #'project-switch-to-buffer
   "pa"  #'+project-add-project
   "pD"  #'+dir-locals-open-or-create
   "p-"  #'project-dired
   "px"  #'project-execute-extended-command
   ;; compile/test
   "pc" #'project-compile
   ;; run
   "pr"  '(nil :wk "run")
   "pre" #'project-eshell
   "prg" #'+project-gdb
   "prs" #'project-shell
   "prc" #'project-shell-command
   "prC" #'project-async-shell-command
   ;; forget
   "pF"  '(nil :wk "forget/cleanup")
   "pFz" #'+project-forget-zombie-projects
   "pFp" #'project-forget-project
   "pFu" #'project-forget-projects-under
   "pFc" #'+project-list-cleanup
   ;; search/replace
   "ps"  '(nil :wk "search/replace")
   "pss" #'project-search
   "psn" '(fileloop-continue :wk "Next match")
   "psr" #'project-query-replace-regexp
   "psf" #'project-find-regexp)

  ;; To handle repeated "SPC u" like repeated "C-u"
  (general-def
   :keymaps 'universal-argument-map
   :prefix minemacs-leader-key
   :global-prefix minemacs-global-mode-prefix
   "u" #'universal-argument-more)

  (when (or os/linux os/bsd)
    (when (executable-find "ecryptfs-verify")
      (+map! "te" #'ecryptfs-toggle-mount-private)))

  ;; Exit minibuffer from anywhere
  (keymap-global-set "S-<escape>" #'+minibuffer-kill-minibuffer)

  ;; HACK: This is a synchronization feature, providing `me-general-ready' tells
  ;; the `+map!', `+map-local!', ... macros that `general' is ready and the
  ;; definers `+minemacs--internal-map!', `+minemacs--internal-map-local!', ...
  ;; are available (See the `+map!' macro definition in "elisp/+minemacs.el").
  (provide 'me-general-ready))



;;; For `me-builtin'

(+evil-conf-for! which-key me-builtin
  :config-form
  (cl-callf append which-key-replacement-alist
    (list
     '(("\\`g z" . "\\`evil-\\(?:mc\\|multiedit\\)-\\(.*\\)")    . (nil . "⌶·\\1"))
     '(("\\`g c" . "\\`evilnc-\\(.*\\)")                         . (nil . "#·\\1"))
     '(("\\`g" . "\\`[Ii]nfo[-:]?\\(?:a-\\)?\\(.*\\)")           . (nil . "ɩ·\\1"))
     '(("\\`SPC TAB" . "\\`tabspaces-\\(.*\\)")                  . (nil . "⭾·\\1"))
     '(("\\`SPC p" . "\\`\\+?\\(?:consult-\\)?project-\\(.*\\)") . (nil . "π·\\1"))
     '(("" . "\\`evil[-:]?\\(?:a-\\)?\\(.*\\)")                  . (nil . "ɛ·\\1")))))

(+map-local! :package edebug
  :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
  "d"   '(nil :wk "edebug")
  "df"  #'edebug-defun
  "dF"  #'edebug-all-forms
  "dd"  #'edebug-all-defs
  "dr"  #'edebug-remove-instrumentation
  "do"  #'edebug-on-entry
  "dO"  #'edebug-cancel-on-entry
  "db"  '(nil :wk "breakpoints")
  "dbb" #'edebug-set-breakpoint
  "dbr" #'edebug-unset-breakpoint
  "dbn" #'edebug-next-breakpoint)

(+map-local! :package edebug
  :keymaps '(edebug-mode-map)
  "e"   '(nil :wk "eval")
  "ee"  #'edebug-eval-last-sexp
  "eE"  #'edebug-eval-expression
  "et"  #'edebug-eval-top-level-form)

(+map-local! :package dired-x
  :keymaps 'dired-mode-map
  "h" #'dired-omit-mode)

(+map-local! :package flymake
  :keymaps 'flymake-mode-map
  "f"  '(nil :wk "flymake")
  "fn" #'flymake-goto-next-error
  "fN" #'flymake-goto-prev-error
  "fs" #'flymake-start
  "fb" #'flymake-show-buffer-diagnostics
  "fp" #'flymake-show-project-diagnostics
  "ff" #'+flymake-transient)

(+map-local! :package reftex
  :keymaps 'reftex-mode-map
  ";" 'reftex-toc)

(with-eval-after-load 'reftex
  (+nvmap! :keymaps 'reftex-toc-mode-map
    "j"   #'next-line
    "k"   #'previous-line
    "q"   #'kill-buffer-and-window
    "ESC" #'kill-buffer-and-window))

(with-eval-after-load 'bibtex
  (+map-local! :keymaps 'bibtex-mode-map
    "l" #'bibtex-fill-entry
    "r" #'bibtex-reformat))

(+map! :package eglot
  :keymaps 'eglot-mode-map
  :infix "c"
  "fF" #'eglot-format-buffer
  "d"  '(eglot-find-declaration :wk "Find declaration")
  "i"  '(eglot-find-implementation :wk "Find implementation")
  "t"  '(eglot-find-typeDefinition :wk "Find type definition")
  "a"  '(eglot-code-actions :wk "Code actions")
  "r"  '(nil :wk "refactor")
  "rr" '(eglot-rename :wk "Rename")
  "rR" '(eglot-code-action-rewrite :wk "Rewrite")
  "rf" '(eglot-code-action-quickfix :wk "Quick fix")
  "ri" '(eglot-code-action-inline :wk "Inline")
  "re" '(eglot-code-action-extract :wk "Extract")
  "ro" '(eglot-code-action-organize-imports :wk "Organize imports")
  "eq" '(eglot-shutdown :wk "Shutdown")
  "er" '(eglot-reconnect :wk "Reconnect")
  "eQ" '(eglot-shutdown-all :wk "Shutdown all")
  "w"  '(eglot-show-workspace-configuration :wk "Eglot workspace config"))

(+map-local! :package elisp-mode
  :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map ielm-map lisp-mode-map racket-mode-map scheme-mode-map)
  "p" #'check-parens)

(+map-local! :package elisp-mode
  :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
  "e"   '(nil :wk "eval")
  "eb"  #'eval-buffer
  "ed"  #'eval-defun
  "ee"  #'eval-last-sexp
  "er"  #'eval-region
  "eR"  #'elisp-eval-region-or-buffer
  "el"  #'load-library
  "g"   '(nil :wk "goto/find")
  "gf"  #'find-function-at-point
  "gR"  #'find-function
  "gv"  #'find-variable-at-point
  "gV"  #'find-variable
  "gL"  #'find-library
  "c"   '(nil :wk "compile")
  "cc"  #'elisp-byte-compile-buffer
  "cf"  #'elisp-byte-compile-file
  "cn"  #'emacs-lisp-native-compile-and-load
  "cb"  #'emacs-lisp-byte-compile-and-load)

(+map! :package smerge-mode
  "gm" '(+smerge-hydra/body :wk "sMerge"))

(+map! :package whitespace
  "tc" #'+whitespace-auto-cleanup-mode)

(+map-local! :package org
  :keymaps 'org-mode-map
  "l"  '(nil :wk "link")
  "ll" #'org-insert-link
  "e"  #'org-export-dispatch
  "c"  #'org-edit-src-code
  "s"  '(nil :wk "babel-session")
  "sc" #'org-babel-switch-to-session-with-code
  "ss" #'org-babel-switch-to-session
  "sp" #'org-babel-pop-to-session
  "sP" #'org-babel-pop-to-session-maybe
  "sl" #'org-babel-load-in-session
  "sL" #'org-babel-load-in-session-maybe
  "si" #'org-babel-initiate-session
  "b"  '(nil :wk "babel")
  "bt" #'org-babel-tangle
  "bd" #'org-babel-detangle
  "bf" #'org-babel-tangle-file)

(+map-local! :package org
  :keymaps 'org-src-mode-map
  "s" #'org-edit-src-save
  "q" #'org-edit-src-abort
  "e" #'org-edit-src-exit)

(+map-local! :package oc
  :keymaps 'org-mode-map "C" #'org-cite-insert)

(+map! :package smerge-mode
  "gm" '(+smerge-hydra/body :wk "sMerge"))

(+map! :package whitespace
  "tc" #'+whitespace-auto-cleanup-mode)

(+map-local! :package bibtex
  :keymaps 'bibtex-mode-map
  "l" #'bibtex-fill-entry
  "r" #'bibtex-reformat)

(when (+emacs-features-p 'sqlite3)
  (+evil-conf-for! sqlite-mode me-builtin
    :config-form
    (+nvmap! :keymaps 'sqlite-mode-map
      "X" #'sqlite-mode-delete
      "d" #'sqlite-mode-list-data
      "t" #'sqlite-mode-list-tables
      "c" #'sqlite-mode-list-columns)))



;;; For `me-multi-cursors'

(use-package evil-multiedit ; This will load `iedit' and suppresses it
  :straight t
  :unless (+package-disabled-p 'iedit 'me-multi-cursors)
  :after evil minemacs-first-file
  :demand
  :init
  (+nvmap! :infix "g"
    "ze" '(nil :wk "evil-multiedit")
    "zee" #'evil-multiedit-match-all
    "zer" #'evil-multiedit-restore
    "zeq" #'evil-multiedit-abort
    "zen" #'evil-multiedit-next
    "zeN" #'evil-multiedit-prev
    "zet" #'evil-multiedit-toggle-or-restrict-region)
  (+vmap! :infix "g"
    "zed" #'evil-multiedit-match-and-next
    "zeD" #'evil-multiedit-match-and-prev)
  (+nmap! :infix "g"
    "zed" #'evil-multiedit-match-symbol-and-next
    "zeD" #'evil-multiedit-match-symbol-and-prev
    "zeT" #'evil-multiedit-toggle-marker-here)
  :config
  (evil-multiedit-default-keybinds))

(use-package evil-iedit-state
  :straight t
  :unless (+package-disabled-p 'iedit 'me-multi-cursors)
  :commands (evil-iedit-state/iedit-mode)
  :after iedit
  :init
  ;; Use the `iedit' key to trigger `evil-iedit-state/iedit-mode'.
  (when iedit-toggle-key-default
    (keymap-global-set (key-description iedit-toggle-key-default) 'evil-iedit-state/iedit-mode))
  :config
  ;; FIX: When we press "C-;" (`iedit-toggle-key-default') to enter `iedit-mode'
  ;; and then "C-;" to quit it, `evil-iedit-state' will stay in `iedit-mode'
  ;; even if the selections aren't displayed and no `iedit' indication is
  ;; displayed in minibuffer.
  (when iedit-toggle-key-default
    (keymap-set evil-iedit-state-map (key-description iedit-toggle-key-default) 'evil-iedit-state/quit-iedit-mode)))

(use-package evil-mc
  :straight t
  :when (memq 'me-multi-cursors minemacs-modules)
  :hook (minemacs-first-file . global-evil-mc-mode)
  :config
  ;; Use "gz" instead of "gr", this last is mapped to `xref-find-references' in some programming modes.
  (evil-define-key* '(normal visual) evil-mc-key-map (kbd "gr") nil)
  (evil-define-key* '(normal visual) evil-mc-key-map (kbd "gz") evil-mc-cursors-map)

  ;; Add support to repeat these commands when prefixed with a number
  (dolist (cmd '(evil-mc-make-and-goto-first-cursor evil-mc-make-and-goto-last-cursor
                 evil-mc-make-and-goto-prev-cursor evil-mc-make-and-goto-next-cursor
                 evil-mc-skip-and-goto-prev-cursor evil-mc-skip-and-goto-next-cursor
                 evil-mc-make-and-goto-prev-match evil-mc-make-and-goto-next-match
                 evil-mc-skip-and-goto-prev-match evil-mc-skip-and-goto-next-match))
    (let ((fn-name (intern (format "+%s--repeat:around-a" cmd))))
      (defalias fn-name (lambda (fn) (dotimes (i (if (integerp current-prefix-arg) current-prefix-arg 1)) (funcall fn))))
      (advice-add cmd :around fn-name)))

  ;; Custom commands to execute with `evil-mc'
  (setq evil-mc-custom-known-commands
        '((backward-kill-word (:default . evil-mc-execute-default-call-with-count))
          (evil-delete-back-to-indentation (:default . evil-mc-execute-default-call))
          (undo-fu-only-redo (:default . evil-mc-execute-default-redo))
          (undo-fu-only-undo (:default . evil-mc-execute-default-undo))
          (corfu-complete (:default . evil-mc-execute-default-complete))
          (evil-numbers/dec-at-pt-incremental (:default . evil-mc-execute-default-call-with-count))
          (evil-numbers/inc-at-pt-incremental (:default . evil-mc-execute-default-call-with-count))
          (evil-org-delete (:default . evil-mc-execute-default-evil-delete))
          (ess-smart-comma (:default . evil-mc-execute-call))
          (evil-digit-argument-or-evil-beginning-of-visual-line
           (:default . evil-mc-execute-default-call)
           (visual . evil-mc-execute-visual-call)))))



;;; For `me-org'

(use-package evil-org
  :straight t
  :hook (org-mode . evil-org-mode))

(use-package evil-org-agenda
  :after evil-org
  :demand
  :config
  (evil-org-agenda-set-keys))



;;; For `me-builtin'

(+evil-conf-for! org me-builtin
  :config-form
  (progn
    ;; RET follows link (Evil equivalent for `org-return-follows-link')
    (+nmap! :keymaps 'org-mode-map "RET" #'org-open-at-point)
    (with-eval-after-load 'evil
      ;; Fix `evil' search problem (to be used with `evil-search')
      (when (eq evil-search-module 'evil-search)
        (setq org-fold-core-style 'overlays)))))

(+evil-conf-for! reftex me-builtin
  :config-form
  (with-eval-after-load 'evil
    (add-hook 'reftex-mode-hook #'evil-normalize-keymaps)))

(+map! :package editorconfig
  "fc" '(editorconfig-find-current-editorconfig :wk "Find current EditorConfig")
  "cfe" #'editorconfig-format-buffer)



;;; For `me-completion'

(+evil-conf-for! corfu me-completion
  :config-form
  (progn
    (keymap-set corfu-map "C-j" #'corfu-next)
    (keymap-set corfu-map "C-k" #'corfu-previous)))

(+evil-conf-for! vertico me-completion
  :config-form
  (progn
    (keymap-set vertico-map "C-j" #'vertico-next)
    (keymap-set vertico-map "C-k" #'vertico-previous)))

(+map! :package consult :module me-completion
  ;; buffer
  "bll" #'consult-line
  "blf" #'consult-focus-lines
  "blk" #'consult-keep-lines
  "blg" #'consult-goto-line
  "bb"  #'consult-buffer
  "bB"  #'consult-buffer-other-window
  "bF"  #'consult-buffer-other-frame
  "bmM" #'consult-bookmark
  "bi"  #'consult-imenu
  "bO"  #'consult-outline
  ;; file
  "fr"  #'consult-recent-file
  ;; git/vc
  "gG"  #'consult-git-grep
  ;; search
  "ss"  (if (executable-find "rg") #'consult-ripgrep #'consult-grep)
  "sS"  (if (executable-find "rg") #'consult-grep #'consult-ripgrep)
  "sf"  (if (executable-find "fd") #'consult-fd #'consult-find)
  "sF"  (if (executable-find "fd") #'consult-find #'consult-fd)
  "sM"  #'consult-man
  "st"  #'consult-locate
  "sh"  #'consult-history
  "sa"  #'consult-org-agenda
  "sl"  #'consult-locate
  "si"  #'consult-isearch-history
  ;; project
  "pl"  #'consult-line-multi
  "pi"  #'consult-imenu-multi
  ;; code
  "cm"  #'consult-flymake
  "cE"  #'consult-compile-error
  ;; extras
  "ec"  #'consult-complex-command
  ;; insert
  "iy"  #'consult-yank-from-kill-ring
  "ip"  #'consult-yank-pop
  "ir"  '(nil :wk "register")
  "irr" #'consult-register
  "irl" #'consult-register-load
  "irs" #'consult-register-store
  ;; help
  "hu"  #'consult-theme
  "hI"  #'consult-info)

(+map-local! :package consult :module me-completion
  :keymaps 'org-mode-map
  "h"   #'consult-org-heading)

(+map! :package consult-dir :module me-completion
  "ed" #'consult-dir)

(+map! :package embark :module me-completion
  "a" #'embark-act
  "A" #'embark-collect)



;;; For `me-natural-langs'

(when (memq 'me-natural-langs minemacs-modules)
  (+nvmap! "z=" #'+spellcheck-correct))

(+map! "ts" #'+spellcheck-mode)



;;; For `me-editor'

(+map! :package vundo :module me-editor
  "ou" #'vundo)

(when (memq 'me-editor minemacs-modules)
  ;; Bind `+yank-region-as-paragraph' (autoloaded from "me-lib.el")
  (+nvmap! "gy" #'+kill-region-as-paragraph))

(+map! :package spdx :module me-editor
  "il" #'spdx-insert-spdx-only
  "ic" #'spdx-insert-spdx-copyright)



;;; For `me-extra'

(+map! :package crux :module me-extra
  "fo" #'crux-open-with
  "fC" #'crux-copy-file-preserve-attributes
  "id" #'crux-insert-date
  "bo" #'crux-kill-other-buffers)



;;; For `me-tools'

(+evil-conf-for! vterm me-tools
  :config-form
  (+imap! :keymaps 'vterm-mode-map
    "C-l" #'vterm-send-right
    "C-h" #'vterm-send-left
    "C-k" #'vterm-send-up
    "C-j" #'vterm-send-down))

(+evil-conf-for! multi-vterm me-tools
  :config-form
  (+nvmap!
    :keymaps 'vterm-mode-map
    ",c" #'multi-vterm
    ",n" #'multi-vterm-next
    ",p" #'multi-vterm-prev
    "<return>" #'evil-insert-resume))

(+map! :package ssh-deploy :module me-tools
  "od" '(ssh-deploy-hydra/body :wk "ssh-deploy"))

(+map! :package tldr :module me-tools
  "ht" #'tldr)

(+map! :package vterm :module me-tools
  "ot" '(nil :wk "vterm")
  "otv" #'+vterm)

(+map! :package multi-vterm :module me-tools
  "otT" #'multi-vterm
  "ott" #'multi-vterm-dedicated-toggle
  "otp" #'multi-vterm-project)

(+map-local! :package journalctl-mode :module me-tools
  :keymaps 'journalctl-mode-map
  "J" #'journalctl-next-chunk
  "K" #'journalctl-previous-chunk)

(+map! :package app-launcher :module me-tools
  "oo" #'app-launcher-run-app)

(+map-local! :package verb :module me-tools
  :keymaps 'verb-mode-map
  "r"     '(nil :wk "verb")
  "r RET" #'verb-send-request-on-point-no-window
  "rs"    #'verb-send-request-on-point-other-window
  "rr"    #'verb-send-request-on-point-other-window-stay
  "rf"    #'verb-send-request-on-point
  "re"    #'verb-export-request-on-point
  "rv"    #'verb-set-var
  "rx"    #'verb-show-vars)



;;; For `me-robot'

(+map! :package ros :module me-robot
  :infix "o"
  "r"  '(nil :wk "ros")
  "rr" '(+hydra-ros-main/body :wk "Hydra")
  "rs" #'ros-set-workspace
  "rp" #'ros-go-to-package
  "rC" #'ros-cache-clean)



;;; For `me-notes'

(+map! :package denote :module me-notes
  :infix "n"
  "n" #'denote-create-note
  "o" #'denote-open-or-create
  "j" #'denote-journal-extras-new-or-existing-entry
  "J" #'denote-journal-extras-new-entry
  "l" #'denote-insert-link
  "L" #'denote-add-links
  "b" #'denote-show-backlinks-buffer)

(+map! :package consult-denote :module me-notes
  :infix "n"
  "f" #'consult-denote
  "s" #'consult-denote-grep)



;;; For `me-media'

(+map! :package empv :module me-media
  :infix "o"
  "v"  '(nil :wk "empv")
  "vp" '(empv-play :wk "Play")
  "vy" '(consult-empv-youtube :wk "Seach Youtube")
  "vr" '(empv-play-radio :wk "Play radio")
  "vs" '(empv-playtlist-save-to-file :wk "Save current playlist")
  "vD" '(+empv-download-playtlist-files :wk "Download current's playlist files"))



;;; For `me-latex'

(+map-local! :package auctex :module me-latex
  :keymaps '(tex-mode-map TeX-mode-map latex-mode-map LaTeX-mode-map)
  "c" #'TeX-command-run-all
  "m" #'TeX-command-master
  "e" #'TeX-engine-set
  "v" #'TeX-view)

(+map-local! :package latex-preview-pane :module me-latex
  :keymaps '(tex-mode-map TeX-mode-map latex-mode-map LaTeX-mode-map)
  "p" #'latex-preview-pane-mode)



;;; For `me-prog'

(use-package evil-textobj-tree-sitter
  :straight (:host github :repo "meain/evil-textobj-tree-sitter" :files (:defaults "queries" "treesit-queries"))
  :when (memq 'me-prog minemacs-modules)
  :after evil minemacs-first-file
  :init
  ;; Require the package on the first `prog-mode' file
  (satch-add-hook 'prog-mode-hook (lambda () (require 'evil-textobj-tree-sitter)) nil nil :transient t)
  :config
  ;; Goto start of next function
  (define-key evil-normal-state-map (kbd "]f") (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "function.outer")))
  ;; Goto start of previous function
  (define-key evil-normal-state-map (kbd "[f") (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "function.outer" t)))
  ;; Goto end of next function
  (define-key evil-normal-state-map (kbd "]F") (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "function.outer" nil t)))
  ;; Goto end of previous function
  (define-key evil-normal-state-map (kbd "[F") (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "function.outer" t t))))

(+evil-conf-for! expreg me-prog
  :init-form
  (+vmap!
    "v" #'expreg-expand
    "q" #'expreg-contract))

(+evil-conf-for! smartparens me-prog
  :config-form
  (with-eval-after-load 'evil-mc
    ;; Make evil-mc cooperate with smartparens better
    (let ((vars (cdr (assq :default evil-mc-cursor-variables))))
      (unless (memq (car sp--mc/cursor-specific-vars) vars)
        (setcdr (assq :default evil-mc-cursor-variables) (append vars sp--mc/cursor-specific-vars))))))

(+map-local! :package ts-movement :module me-prog
  :keymaps 'ts-movement-map "v" #'+ts-movement-transient)

(+map! :package consult-eglot :module me-prog
  :keymaps 'eglot-mode-map
  "cs" '(consult-eglot-symbols :wk "Symbols"))

(+map! :package apheleia :module me-prog
  "cff" #'apheleia-format-buffer)

(+map! :package quickrun :module me-prog
  "cq"  '(nil :wk "quickrun")
  "cqq" #'quickrun
  "cqQ" #'quickrun-select
  "cqs" #'quickrun-shell
  "cqa" #'quickrun-with-arg
  "cqc" #'quickrun-compile-only
  "cqC" #'quickrun-compile-only-select
  "cqd" #'quickrun-select-default)

(+map! :package rainbow-mode :module me-prog
  :keymaps '(prog-mode-map conf-mode-map text-mode-map)
  "tR" #'rainbow-mode)

(+map! :package devdocs :module me-prog
  "hhh" #'devdocs-lookup
  "hhp" #'devdocs-peruse
  "hhs" #'devdocs-search
  "hhI" #'devdocs-install)



;;; For `me-search'

(+evil-conf-for! dogears me-search
  :init-form
  (progn
    (keymap-global-set "<remap> <evil-jump-forward>" #'dogears-forward)
    (keymap-global-set "<remap> <evil-jump-backward>" #'dogears-back)
    (keymap-global-set "<remap> <xref-pop-marker-stack>" #'dogears-back)))

(+map! :package rg :module me-search
  "sr" #'rg-dwim
  "sR" #'rg)

(+map! :package fzf :module me-search
  "/"   #'fzf-project
  "sz" '(nil :wk "fzf")
  "szz" #'fzf
  "szg" #'fzf-grep
  "szG" #'fzf-grep-dwim
  "szf" #'fzf-find-file
  "szF" #'fzf-find-file-in-dir)



;;; For `me-math'

(+map! :package ein :module me-math
  :infix "o"
  "j" '(nil :wk "ein")
  "jr" #'ein:run
  "jl" #'ein:login
  "jf" #'ein:file-open
  "jn" #'ein:notebook-open)

(+map-local! :package ein :module me-math
  :keymaps 'ein:ipynb-mode-map
  "o" #'ein:process-find-file-callback
  "O" #'ein:process-open-notebook
  "r" #'ein:gat-run-remote
  "l" #'ein:gat-run-local)



;;; For `me-files'

(+evil-conf-for! dirvish obsolete/me-dirvish
  :config-form
  (+nvmap! :keymaps 'dirvish-mode-map
    "q" #'dirvish-quit
    "s" #'dirvish-subtree-toggle
    "y" #'dirvish-yank-menu))

(+map! :package dirvish :module obsolete/me-dirvish
  ;; Open
  "o-" #'dirvish
  "oq" #'dirvish-quick-access
  ;; Search
  "sd" #'dirvish-fd)

(+map! :package neotree :module me-files
  "op" #'neotree-toggle)

(+map! :package sudo-edit :module me-files
  "fF" #'sudo-edit-find-file
  "fu" #'sudo-edit)

(+map! :package ztree :module me-files
  "oz" #'ztree-diff)




;;; For `me-email'

(+evil-conf-for! mu4e me-email
  :config-form
  (progn
    (+nvmap! :keymaps 'mu4e-view-mode-map "p" #'mu4e-view-save-attachments)
    (+nvmap! :keymaps '(mu4e-headers-mode-map mu4e-view-mode-map)
      "gw" #'+mu4e-open-mail-as-html
      "g RET" #'browse-url-at-point)))

(+evil-conf-for! org-msg me-email
  :config-form
  (+nvmap! :keymaps 'org-msg-edit-mode-map "gg" #'org-msg-goto-body))

(+map! :package mu4e :module me-email
  "om" #'+mu4e)

(+map-local! :package mu4e :module me-email
  :keymaps '(mu4e-compose-mode-map org-msg-edit-mode-map)
  "s" #'message-send-and-exit
  "d" #'message-kill-buffer
  "S" #'message-dont-send)

(+map-local! :package org-msg :module me-email
  :keymaps 'org-msg-edit-mode-map
  "a"  '(nil :wk "attach")
  "aa" #'org-msg-attach-attach
  "ad" #'org-msg-attach-delete
  "k"  #'org-msg-edit-kill-buffer
  "p"  #'org-msg-preview)



;;; For `me-debug'

(+map! :package dape :module me-debug
  :infix "d"
  "d" #'dape
  "n" #'dape-next
  "s" #'dape-step-in
  "o" #'dape-step-out
  "c" #'dape-continue
  "r" #'dape-restart
  "p" #'dape-pause
  "b" #'dape-breakpoint-toggle
  "e" #'dape-breakpoint-expression
  "r" #'dape-remove-breakpoint-at-point
  "R" #'dape-breakpoint-remove-all
  "t" #'+dape-transient
  "q" #'dape-kill
  "Q" #'dape-quit)

(+map-local! :package disaster :module me-debug
  :keymaps '(c-mode-map c++-mode-map fortran-mode-map)
  "D" #'disaster)



;;; For on-demand modules

(+map-local! :package sly
  :keymaps '(lisp-mode-map)
  "s"  #'sly
  "c"  '(nil :wk "compile")
  "cc" #'sly-compile-file
  "cC" #'sly-compile-and-load-file
  "cd" #'sly-compile-defun
  "cr" #'sly-compile-region
  "g"  '(nil :wk "goto/find")
  "gn" #'sly-goto-first-note
  "gL" #'sly-load-file
  "gn" #'sly-next-note
  "gN" #'sly-previous-note
  "gs" #'sly-stickers-next-sticker
  "gS" #'sly-stickers-prev-sticker
  "gN" #'sly-previous-note
  "gd" #'sly-edit-definition
  "gD" #'sly-edit-definition-other-window
  "gb" #'sly-pop-find-definition-stack
  "h"  '(nil :wk "help/info")
  "hs" #'sly-describe-symbol
  "hf" #'sly-describe-function
  "hc" #'sly-who-calls
  "hC" #'sly-calls-who
  "hs" #'sly-who-calls
  "hC" #'sly-calls-who
  "hd" #'sly-disassemble-symbol
  "hD" #'sly-disassemble-definition
  "r"  '(nil :wk "repl")
  "rr" #'sly-restart-inferior-lisp
  "rc" #'sly-mrepl-clear-repl
  "rs" #'sly-mrepl-sync
  "rn" #'sly-mrepl-new
  "rq" #'sly-quit-lisp)

(+map-local! :package sly-macrostep :module me-common-lisp
  :keymaps '(sly-mode-map sly-editing-mode-map sly-mrepl-mode-map)
  "m" '(macrostep-expand :wk "Expand macro"))

(+evil-conf-for! nov on-demand/me-epub
  :config-form
  (+nmap! :keymaps 'nov-mode-map "RET" #'nov-scroll-up))

(+map-local! :package markdown-mode
  :keymaps 'markdown-mode-map
  "l"  '(nil :wk "link")
  "ll" #'markdown-insert-link
  "e"  #'markdown-export)

(+map-local! :package macrostep-geiser
  :keymaps '(geiser-mode-map geiser-repl-mode-map)
  "m" '(macrostep-expand :wk "Expand macro")
  "M" #'macrostep-geiser-expand-all)

(+map-local! :package rust-mode
  :keymaps '(rust-mode-map rust-ts-mode-map)
  "c" #'rust-compile
  "C" #'rust-compile-release
  "k" #'rust-check
  "t" #'rust-test
  "r" #'rust-run
  "R" #'rust-run-release
  "y" #'rust-run-clippy
  "f" #'rust-format-buffer
  "F" #'rust-goto-format-problem
  "S" #'rust-enable-format-on-save)

(+map! :package docker
  "ok" #'docker)

(+map-local! :package pkgbuild-mode
  :keymaps 'pkgbuild-mode-map
  "b" #'pkgbuild-makepkg
  "a" #'pkgbuild-tar
  "r" #'pkgbuild-increase-release-tag
  "u" #'pkgbuild-browse-url
  "m" #'pkgbuild-update-sums-line
  "s" #'pkgbuild-update-srcinfo
  "e" #'pkgbuild-etags)

(+map-local! :package scad-mode
  :keymaps 'scad-mode-map
  "p" #'scad-preview)

(+map-local! :package mermaid-mode
  :keymaps 'mermaid-mode-map
  "c" 'mermaid-compile
  "f" 'mermaid-compile-file
  "b" 'mermaid-compile-buffer
  "r" 'mermaid-compile-region
  "b" 'mermaid-open-browser
  "d" 'mermaid-open-doc)

(+map-local! :package d2-mode
  :keymaps 'd2-mode-map
  "cc" #'d2-compile
  "cf" #'d2-compile-file
  "cb" #'d2-compile-buffer
  "cr" #'d2-compile-region
  "cF" #'d2-compile-file-and-browse
  "cB" #'d2-compile-buffer-and-browse
  "cR" #'d2-compile-region-and-browse
  "o"  #'d2-open-browser
  "v"  #'d2-view-current-svg
  "h"  #'d2-open-doc)

(+map-local! :package csv-mode
  :keymaps 'csv-mode-map
  "a" #'csv-align-fields
  "u" #'csv-unalign-fields
  "s" #'csv-sort-fields
  "S" #'csv-sort-numeric-fields
  "k" #'csv-kill-fields
  "t" #'csv-transpose)

(+map-local! :package rainbow-csv
  :keymaps '(csv-mode-map tsv-mode-map)
  "r" #'rainbow-csv-mode
  "R" #'rainbow-csv-highlight)

(+map-local! :package json-mode
  :keymaps '(json-mode-map json-ts-mode-map)
  "p" #'json-mode-show-path
  "t" #'json-toggle-boolean
  "d" #'json-mode-kill-path
  "x" #'json-nullify-sexp
  "+" #'json-increment-number-at-point
  "-" #'json-decrement-number-at-point
  "f" #'json-mode-beautify)

(+map-local! :package graphviz-dot-mode
  :keymaps 'graphviz-dot-mode-map
  "p" #'graphviz-dot-preview
  "P" #'graphviz-dot-view
  "l" #'graphviz-turn-on-live-preview
  "L" #'graphviz-turn-off-live-preview)

(+map-local! :package plantuml-mode
  :keymaps 'plantuml-mode-map
  "p" #'plantuml-preview-buffer
  "P" #'plantuml-preview
  "d" `(,(+cmdfy! (if plantuml-mode-debug-enabled
                      (plantuml-disable-debug)
                    (plantuml-enable-debug)))
        :wk "Toggle debug"))



;;; For `me-rss'

(+evil-conf-for! elfeed me-rss
  :config-form
  (progn
    (+nmap! :keymaps 'elfeed-search-mode-map "d" #'+elfeed-youtube-dl)
    (+nmap! :keymaps 'elfeed-show-mode-map "D" #'+elfeed-download-image)))

(+map! :package elfeed :module me-rss
  "of" #'+elfeed)



;;; For `me-org'

(+evil-conf-for! xkcd me-fun
  :config-form
  (+nvmap! :keymaps 'xkcd-mode-map
    "j" #'xkcd-next
    "k" #'xkcd-prev
    "l" #'xkcd-get-latest
    "L" #'xkcd-get-latest-cached
    "<right>" #'xkcd-next
    "<left>" #'xkcd-prev
    "o" #'xkcd-open-browser
    "O" #'xkcd-open-explanation-browser
    "r" #'xkcd-rand
    "y" #'xkcd-copy-link))

(+map! :package xkcd :module me-fun
  "ox" #'xkcd)



;;; For `me-vc'

(+evil-conf-for! git-commit me-vc
  :config-form
  (with-eval-after-load 'evil (evil-set-initial-state 'git-commit-mode 'insert)))

(+evil-conf-for! diffview me-vc
  :config-form
  (+nvmap! :keymaps 'diffview--mode-map
    "="   #'diffview--align-windows
    "+"   #'diffview--align-windows
    "C-j" #'diffview--next-file
    "C-k" #'diffview--prev-file
    "q"   #'diffview--quit))

(+map! :package magit :module me-vc
  :infix "g"
  "g" #'magit-status
  "C" #'magit-clone
  "b" #'magit-blame
  "l" #'magit-log
  "d" #'magit-diff-dwim
  "s" #'magit-stage
  "i" #'magit-init)

(+map-local! :package magit-todos :module me-vc
  :keymaps 'magit-status-mode-map
  "t" `(,(+cmdfy! (magit-todos-mode 'toggle) (magit-refresh)) :wk "magit-todos-mode"))

(+map! :package forge :module me-vc
  :infix "g"
  "f" '(nil :wk "forge")
  "ff" #'forge-dispatch
  "fc" #'forge-create-post
  "fe" #'forge-edit-post
  "ft" #'forge-edit-topic-title
  "fs" #'forge-edit-topic-state
  "fd" #'forge-edit-topic-draft)

(+map! :package diff-hl :module me-vc
  "gs" #'diff-hl-stage-current-hunk)

(+map! :package git-timemachine :module me-vc
  "gt" #'git-timemachine-toggle)

(+map! :package repo :module me-vc
  "gr" '(nil :wk "repo")
  "grg" #'repo-status)

(+map! :package repo-transient :module me-vc
  "grr" #'repo-main-menu)

(with-eval-after-load 'diff-mode
  (+map-local! :package diffview :module me-vc
    :keymaps 'diff-mode-map
    "v" #'diffview-current
    "V" #'diffview-region))



;;; For `me-ui'

(+evil-conf-for! pulsar me-ui
  :config-form
  (with-eval-after-load 'evil
    (cl-callf append pulsar-pulse-functions
      '(evil-yank evil-paste-after evil-paste-before
        evil-delete evil-delete-line evil-delete-whole-line
        evil-goto-last-change evil-goto-last-change-reverse))))

(+map! :package focus :module me-ui
  "tF" #'focus-mode)

(+map! :package me-writing-mode :module me-ui
  "tw" #'+writing-mode
  "tW" #'+writing-global-mode)

(+evil-conf-for! mixed-pitch me-ui
  :init-form
  (+map! "tm" #'mixed-pitch-mode))



;;; For `me-god'

(+evil-conf-for! god-mode me-god
  :config-form
  (with-eval-after-load 'evil
    (evil-make-intercept-map god-local-mode-map 'normal)
    (add-hook 'god-local-mode-hook #'evil-normalize-keymaps)))



;;; For `me-emacs-lisp'

(with-eval-after-load 'parinfer-rust-mode
  ;; The `evil-shif-right' (and `evil-shift-left' which uses it under the hood)
  ;; behave strangely when `parinfer-rust-mode' is enabled, so lets disable when
  ;; using this command.
  (when (fboundp '+parinfer-rust--disable) ; defined in `me-emacs-lisp'
    (with-eval-after-load 'evil
      (advice-add #'evil-shift-right :before #'+parinfer-rust--disable)
      (advice-add #'evil-shift-right :after #'+parinfer-rust--restore))))

(+map-local! :package macrostep :module me-emacs-lisp
  :keymaps '(emacs-lisp-mode-map lisp-mode-map)
  "m" '(macrostep-expand :wk "Expand macro"))

(+map! :package macrostep :module me-emacs-lisp
  :keymaps 'emacs-lisp-mode-map
  :infix "h"
  "p" #'helpful-at-point
  "o" #'helpful-symbol
  "c" #'helpful-command
  "F" #'helpful-function
  "f" #'helpful-callable)

(with-eval-after-load 'octave
  (+map-local! :package eros :module me-emacs-lisp
    :keymaps 'octave-mode-map
    "e"  '(nil :wk "eval")
    "ee" #'+eros-octave-eval-last-sexp))



;;; For `me-embedded'

(+map! :package embed :module me-embedded
  :infix "o"
  "b" '(nil :wk "embed")
  "bo" #'embed-openocd-start
  "bO" #'embed-openocd-stop
  "bg" #'embed-openocd-gdb
  "bf" #'embed-openocd-flash)

(+map-local! :package bitbake :module me-embedded
  :keymaps 'bitbake-mode-map
  "b"  #'bitbake-recipe-build-dir-dired
  "ii" #'bitbake-inc-pr
  "iv" #'bitbake-insert-var
  "ia" #'bitbake-append-var
  "io" #'bitbake-insert-override)



;;; For `me-checkers'

(+evil-conf-for! flymenu me-checkers
  :init-form
  (+map! "cM" #'flymenu-flymake))



;;; For `me-calendar'

(+map! :package calfw :module me-calendar
  "oC" #'+cfw:open-calendar-buffer)



;;; For `me-project'

(+map! :package consult-project-extra :module me-project
  :infix "p" ;; project
  "p" #'consult-project-extra-find
  "P" #'consult-project-extra-find-other-window)

(+map! :package projection-dape :module me-project
  "dD" #'projection-dape)

(+map! :package projection-multi :module me-project
  "pC" #'projection-multi-compile)



;;; For `me-window'

(+map! :package ace-window :module me-window
  "wa" #'ace-window)

(+map! "wj" '(+window-adjust-size-transient :wk "+window-adjust-size"))



;;; For obsolete modules/packages

(+evil-conf-for! eopengrok obsolete/me-eopengrok
  :config-form
  (+nmap!
    :keymaps 'eopengrok-mode-map
    "n" #'eopengrok-next-line
    "p" #'eopengrok-previous-line
    "q" #'eopengrok-quit
    "RET" #'eopengrok-jump-to-source))

(+map! :package clang-format :module obsolete/me-clang-format
  :keymaps '(c-mode-map c++-mode-map c-ts-mode-map c++-ts-mode-map cuda-mode-map scad-mode-map)
  "cfc" #'clang-format-buffer)

(+evil-conf-for! better-jumper obsolete/me-better-jumper
  :init-form
  (progn
    (keymap-global-set "<remap> <evil-jump-forward>" #'better-jumper-jump-forward)
    (keymap-global-set "<remap> <evil-jump-backward>" #'better-jumper-jump-backward)
    (keymap-global-set "<remap> <xref-pop-marker-stack>" #'better-jumper-jump-backward)))

(+map-local! :package restclient :module obsolete/me-restclient
  :keymaps 'restclient-mode-map
  "r"     '(nil :wk "restclinet")
  "r RET" #'restclient-http-send-current-suppress-response-buffer
  "rs"    #'restclient-http-send-current
  "rr"    #'restclient-http-send-current-stay-in-window
  "rf"    #'restclient-http-send-current-raw
  "re"    #'restclient-copy-curl-command)

(+map-local! :package dap-mode :module obsolete/me-lsp
  :keymaps '(c-mode-map c++-mode-map python-mode-map
             rust-mode-map sh-mode-map bash-ts-mode-map
             js-mode-map js-ts-mode-map ruby-mode-map
             perl-mode-map)
  "d" '(nil :wk "dap")
  "dd" #'dap-debug
  "dt" #'dap-debug-edit-template
  "dh" #'dap-hydra/body)

(+map! :package  consult-lsp :module obsolete/me-lsp
  :keymaps 'lsp-mode-map
  "cs" '(consult-lsp-file-symbols :wk "Symbols"))

(+map! :package lsp-mode :module obsolete/me-lsp
  :infix "c"
  "l"  '(nil :wk "lsp session")
  "ll" #'lsp
  "lA" #'+lsp-auto-enable)

(+map! :package lsp-mode :module obsolete/me-lsp
  :keymaps 'lsp-mode-map
  :infix "c"
  "fF" #'lsp-format-buffer
  "d"  '(lsp-find-declaration :wk "Find declaration")
  "D"  '(lsp-find-definition :wk "Find definition")
  "i"  '(lsp-find-implementation :wk "Find implementation")
  "t"  '(lsp-find-type-definition :wk "Find type definition")
  "a"  '(lsp-execute-code-action :wk "Code actions")
  "r"  '(nil :wk "refactor")
  "rr" '(lsp-rename :wk "Rename")
  "lq" '(lsp-workspace-shutdown :wk "Shutdown")
  "lr" '(lsp-workspace-restart :wk "Restart"))

(+map! :package eaf :module obsolete/me-eaf
  "oo" #'eaf-open)

(+evil-conf-for! eaf obsolete/me-eaf
  :init-form
  ;; Evil integration doesn't work, start `eaf-mode' in `emacs-state'.
  (with-eval-after-load 'evil
    (evil-set-initial-state 'eaf-mode 'emacs)))

(+evil-conf-for! expand-region obsolete/me-expand-region
  :init-form
  (+vmap!
    "v" #'er/expand-region
    "q" #'er/contract-region))

(+map! :package guix :module obsolete/me-packages-managers
  "og" #'guix)

(+map! :package makefile-executor :module obsolete/me-makefile-executor
  "pm" '(nil :wk "makefile-executor")
  "pmm" #'makefile-executor-execute-project-target
  "pml" #'makefile-executor-execute-last)

(+map-local! :package makefile-executor :module obsolete/me-makefile-executor
  :keymaps 'makefile-mode-map
  "pmt" #'makefile-executor-execute-target
  "pmb" #'makefile-executor-execute-dedicated-buffer)

(+map! :package project-cmake :module obsolete/me-project-cmake
  :keymaps '(c-mode-map c++-mode-map c-ts-mode-map c++-ts-mode-map)
  :infix "p"
  "m" '(nil :wk "project-cmake")
  "mb" #'project-cmake-build
  "mg" #'project-cmake-configure
  "mt" #'project-cmake-test
  "mI" #'project-cmake-install
  "ms" #'project-cmake-scan-kits
  "mS" #'project-cmake-shell)

(+map-local! :package go-translate :module obsolete/me-go-translate
  :keymaps '(org-mode-map text-mode-map markdown-mode-map tex-mode-map TeX-mode-map latex-mode-map LaTeX-mode-map)
  "t" '(nil :wk "translate")
  "tb" `(,(+cmdfy! (+gts-translate-with 'bing)) :wk "Translate with Bing")
  "td" `(,(+cmdfy! (+gts-translate-with 'deepl)) :wk "Translate with DeepL")
  "tg" `(,(+cmdfy! (+gts-translate-with 'google)) :wk "Translate with Google")
  "tr" #'+gts-yank-translated-region
  "tt" #'+gts-translate-with
  "tT" #'gts-do-translate)

(+map! :package org-present :module obsolete/me-org-present
  "oP" :keymaps 'org-mode-map #'org-present)

(+map! :package elisp-demos :module obsolete/me-elisp-demos
  :infix "he"
  "d" #'elisp-demos-find-demo
  "D" #'elisp-demos-add-demo)

(when (< emacs-major-version 29)
  (+map! :package emojify :module obsolete/me-code-review
    "ie" '(emojify-insert-emoji :wk "Emoji")))

(+map! :package projectile :module obsolete/me-projectile
  ;; Project
  :infix "p"
  "a"  '(projectile-add-known-project :wk "Add")
  "D"  '(projectile-edit-dir-locals :wk "Edit dir-locals")
  "<" #'projectile-switch-open-project
  ;; Compile/test
  "c"  '(nil :wk "compile/test")
  "cc" #'projectile-compile-project
  "cg" #'projectile-configure-project
  "ct" #'projectile-test-project
  "ci" #'projectile-install-project
  "cp" #'projectile-package-project
  "r"  '(nil :wk "run")
  "rr" #'projectile-run-project
  "rg" #'projectile-run-gdb
  "rt" #'projectile-run-vterm
  "re" #'projectile-run-eshell
  "rs" #'projectile-run-shell
  "rR" #'projectile-run-command-in-root
  "rS" #'projectile-run-shell-command-in-root
  "rA" #'projectile-run-async-shell-command-in-root
  ;; Forget
  "F"  '(nil :wk "forget/cleanup")
  "Fz" '(projectile-cleanup-known-projects :wk "Cleanup zombie projects")
  "Fp" '(projectile-remove-known-project :wk "Forget project")
  "FP" '(projectile-remove-current-project-from-known-projects :wk "Forget current project")
  "Fc" #'projectile-invalidate-cache
  ;; Search/replace
  "s"  '(nil :wk "search/replace")
  "ss" 'projectile-grep
  "sn" '(fileloop-continue :wk "Next match")
  "sr" #'projectile-replace-regexp)

(+map! :package consult-projectile :module obsolete/me-projectile
  ":"  '(consult-projectile-find-file :wk "Find file in project")
  ;; Buffer
  "bp" #'consult-projectile-switch-to-buffer
  ;; Project
  "pp" #'consult-projectile
  "pP" '(consult-projectile-switch-project :wk "Switch")
  "pR" #'consult-projectile-recentf
  "pd" '(consult-projectile-find-dir :wk "Find directory")
  "pf" '(consult-projectile-find-file :wk "Find file"))

(+evil-conf-for! projectile obsolete/me-projectile
  :init-form
  (with-eval-after-load 'evil
    (keymap-global-set "<remap> <evil-jump-to-tag>" 'projectile-find-tag)))

(+map! :package writeroom-mode :module obsolete/me-writeroom
  "tw" #'writeroom-mode)

(+map! :package tabspaces :module obsolete/me-tabspaces
  :infix "q"
  "t" #'tabspaces-save-session
  "T" #'tabspaces-restore-session
  "p" #'tabspaces-save-current-project-session)

(+map! :package tabspaces :module obsolete/me-tabspaces
  :infix "TAB"
  "TAB" '(tabspaces-switch-or-create-workspace :w "Switch or create")
  "o" '(tabspaces-open-or-create-project-and-workspace :wk "Open or create project")
  "f" '(tabspaces-project-switch-project-open-file :wk "Switch project & open file")
  "d" #'tabspaces-close-workspace
  "b" #'tabspaces-switch-to-buffer
  "t" #'tabspaces-switch-buffer-and-tab
  "C" #'tabspaces-clear-buffers
  "r" #'tabspaces-remove-current-buffer
  "R" #'tabspaces-remove-selected-buffer
  "k" #'(tabspaces-kill-buffers-close-workspace :wk "Kill buffers & close WS"))

(+map! :package dashboard :module obsolete/me-dashboars
  "oD" #'dashboard-open)

;; Ensure setting the keybindings before opening the dashboard
(+evil-conf-for! dashboard obsolete/me-dashboard
  :init-form
  (with-eval-after-load 'evil (evil-collection-dashboard-setup)))

(+map! :package org-roam :module obsolete/me-org-roam
  :infix "n"
  "f" #'org-roam-node-find
  "r" #'org-roam-ref-find
  "i" #'org-roam-node-insert
  "R" #'org-roam-node-random
  "B" #'org-roam-buffer-display-dedicated)

(+map! :package org-roam-ui :module obsolete/me-org-roam
  "nu" #'org-roam-ui-open)

(+map! :package consult-org-roam :module obsolete/me-org-roam
  :infix "n"
  "s" #'consult-org-roam-search
  "l" #'consult-org-roam-forward-links
  "b" #'consult-org-roam-backlinks
  "F" #'consult-org-roam-file-find)

(+map! :package flycheck :module obsolete/me-flycheck
  "tc" #'flycheck-mode)

(+map! :package flycheck :module obsolete/me-flycheck
  :keymaps 'flycheck-error-list-mode-map
  "j"   #'flycheck-error-list-next-error
  "k"   #'flycheck-error-list-previous-error
  "RET" #'flycheck-error-list-goto-error)

(+map-local! :package realgud :module obsolete/me-realgud
  :keymaps '(c-mode-map c++-mode-map python-mode-map
             c-ts-mode-map c++-ts-mode-map python-ts-mode-map
             rust-mode-map rust-ts-mode-map
             sh-mode-map bash-ts-mode-map)
  "r" '(nil :wk "realgud")
  "rd" #'+realgud:start
  "rh" #'+realgud-hydra/body)

(+map! :package chezmoi-ediff :module obsolete/me-chezmoi
  "oce" #'chezmoi-ediff)

(+map! :package chezmoi-magit :module obsolete/me-chezmoi
  "ocg" #'chezmoi-magit-status)

(+map! :package chezmoi :module obsolete/me-chezmoi
  :infix "o"
  "c" '(nil :wk "chezmoi")
  "cf" #'chezmoi-find
  "cw" #'chezmoi-write
  "cd" #'chezmoi-diff
  "co" #'chezmoi-open-other
  "cs" #'chezmoi-sync-files)

(+evil-conf-for! blamer obsolete/me-blamer
  :config-form
  (+nvmap!
    "gb" #'blamer-show-posframe-commit-info
    "gB" #'blamer-show-commit-info))

(+map-local! :package zotxt :module obsolete/me-zotxt
  :keymaps 'org-mode-map
  "z" #'org-zotxt-mode)

(+map-local! :package zotxt :module obsolete/me-zotxt
  :keymaps 'markdown-mode-map
  "z" #'zotxt-citekey-mode)

(+map! :package lexic :module obsolete/me-lexic
  :infix "s"
  "l" #'lexic-search-word-at-point
  "L" #'lexic-search)

(+evil-conf-for! lexic obsolete/me-lexic
  :config-form
  (+nvmap! :keymaps 'lexic-mode-map
    "q" #'lexic-return-from-lexic
    "RET" #'lexic-search-word-at-point
    "a" #'outline-show-all
    "h" `(,(+cmdfy! (outline-hide-sublevels 3)) :wk "Hide sublevels")
    "o" #'lexic-toggle-entry
    "n" #'lexic-next-entry
    "N" `(,(+cmdfy! (lexic-next-entry t)) :wk "Last entry")
    "p" #'lexic-previous-entry
    "P" `(,(+cmdfy! (lexic-previous-entry t)) :wk "First entry")
    "E" `(,(+cmdfy!
            (lexic-return-from-lexic)
            (switch-to-buffer (lexic-get-buffer)))
          :wk "Expand")
    "M" `(,(+cmdfy!
            (lexic-return-from-lexic)
            (lexic-goto-lexic))
          :wk "Minimise")
    "C-p" #'lexic-search-history-backwards
    "C-n" #'lexic-search-history-forwards
    "/" `(,(+cmdfy! (call-interactively #'lexic-search)) :wk "Search")))


(provide 'obsolete/me-evil)
;;; me-evil.el ends here
