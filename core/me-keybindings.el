;;; me-keybindings.el --- Default keybindings -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(use-package which-key
  :straight t
  :hook (minemacs-after-startup . which-key-mode)
  :custom
  (which-key-idle-delay 1.0)
  (which-key-idle-secondary-delay nil)
  (which-key-ellipsis "..")
  (which-key-prefix-prefix "+")
  (which-key-sort-order #'which-key-key-order-alpha)
  (which-key-min-display-lines 3)
  (which-key-max-display-columns nil)
  ;; Allow a key binding to be modified by multiple rules in
  ;; `which-key-replacement-alist'
  (which-key-allow-multiple-replacements t)
  :config
  (setq
   which-key-replacement-alist
   (append
    which-key-replacement-alist
    (list
     '(("\\`g z" . "\\`evil-\\(?:mc\\|multiedit\\)-\\(.*\\)")    . (nil . "⌶·\\1"))
     '(("\\`g c" . "\\`evilnc-\\(.*\\)")                         . (nil . "#·\\1"))
     '(("\\`g" . "\\`[Ii]nfo[-:]?\\(?:a-\\)?\\(.*\\)")           . (nil . "ɩ·\\1"))
     '(("\\`SPC TAB" . "\\`tabspaces-\\(.*\\)")                  . (nil . "⭾·\\1"))
     '(("\\`SPC p" . "\\`\\+?\\(?:consult-\\)?project-\\(.*\\)") . (nil . "π·\\1"))
     '(("" . "\\`evil[-:]?\\(?:a-\\)?\\(.*\\)")                  . (nil . "ɛ·\\1")))))
  ;; Setup `which-key' integration with the minibuffer
  (which-key-setup-minibuffer))

(use-package general
  :straight t
  ;; PERF: Loading `general' early make Emacs very slow on startup.
  :after evil
  :demand t
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
    "fi"   #'auto-insert
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
    "iu"   '(insert-char :wk "Unicode char")
    "ip"   #'yank-pop ;; Will be overwritten with `consult-yank-pop'
    "ie"   `(,(when (>= emacs-major-version 29) #'emoji-search) :wk "Emoji")

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
    "o-"   '(dired :wk "Dired") ;; Will be overwritten if dirvish is used
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
    (when (executable-find "netExtender")
      (+map! "tV" #'netextender-toggle))
    (when (executable-find "ecryptfs-verify")
      (+map! "te" #'ecryptfs-toggle-mount-private)))

  ;; Exit minibuffer from anywhere
  (keymap-global-set "S-<escape>" #'+minibuffer-kill-minibuffer)

  ;; HACK: This is a synchronization feature, providing `me-general-ready' tells
  ;; the `+map!', `+map-local!', ... macros that `general' is ready and the
  ;; definers `+minemacs--internal-map!', `+minemacs--internal-map-local!', ...
  ;; are available (See the `+map!' macro definition in "elisp/+minemacs.el").
  (provide 'me-general-ready))

(use-package hydra
  :straight t)

(use-package avy
  :straight t
  :bind (("C-\"" . avy-goto-char)
         ("C-é" . avy-goto-line) ; French AZERTY
         ("M-g l" . avy-goto-line)))


(provide 'me-keybindings)

;;; me-keybindings.el ends here
