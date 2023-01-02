;;; me-keybindings.el --- Default keybindings -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


;; Which key
(use-package which-key
  :straight t
  :hook (minemacs-after-startup . which-key-mode)
  :custom
  (which-key-idle-delay 1.0)
  (which-key-idle-secondary-delay 0.05)
  (which-key-prefix-prefix "↪ ")
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-min-display-lines 3)
  (which-key-max-display-columns nil)
  (which-key-allow-multiple-replacements t)
  :config
  ;; g, [, ] (\\`[][g])
  (push '(("" . "\\`evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "🅴·\\1"))
        which-key-replacement-alist)
  ;; g
  (push '(("\\`g" . "\\`Info[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "🅘·\\1"))
        which-key-replacement-alist)
  ;; g z
  (push '(("\\`g z" . "\\`evil-mc-\\(.*\\)") . (nil . "⌶·\\1"))
        which-key-replacement-alist)
  ;; g c
  (push '(("\\`g c" . "\\`evilnc-\\(.*\\)") . (nil . "#·\\1"))
        which-key-replacement-alist)

  (which-key-setup-minibuffer))

;;; General.el
(use-package general
  :straight t
  :config
  (general-auto-unbind-keys)

  ;; Global leader
  (general-create-definer +map
    ;; The order of states matters, the last is prioritized
    :states '(insert emacs visual normal)
    :keymaps 'override
    :prefix minemacs-leader-key
    :global-prefix minemacs-global-leader-prefix)

  ;; Local leader
  (general-create-definer +map-local
    :states '(insert emacs visual normal)
    :keymaps 'override
    :prefix minemacs-localleader-key
    :global-prefix minemacs-global-mode-prefix)

  ;; Map a key in normal and visual states
  (general-create-definer +map-key
    :states '(normal visual))

  (+map
    ;; ====== Top level functions ======
    "TAB" '(switch-to-next-buffer :wk "Next buffer")
    "<backtab>" '(switch-to-prev-buffer :wk "Previous buffer")
    "SPC" '(execute-extended-command :wk "M-x")
    ";"   '(pp-eval-expression :wk "Eval expression")
    "X"   #'org-capture
    "u"   '(universal-argument :wk "C-u")

    ;; ====== Quit/Session ======
    "q"   '(nil :wk "quit/session")
    "qq"  #'save-buffers-kill-terminal
    "qQ"  #'kill-emacs
    "qS"  #'server-start
    "qR"  #'recover-session
    "qd"  #'desktop-read
    "qD"  #'desktop-lazy-complete
    "qs"  #'desktop-save

    ;; ====== Files ======
    "f"   '(nil :wk "file")
    "fS"  '(write-file :wk "Save as ...")
    "fD"  #'+delete-this-file
    "fu"  #'+sudo-find-file
    "fU"  #'+sudo-this-file
    "fR"  #'+move-this-file
    "ff"  #'find-file
    "fs"  #'save-buffer
    "ft"  #'recover-this-file
    "fT"  #'recover-file
    "fy"  #'+yank-this-file-name
    "fE"  `(,(+cmdfy! (dired (or minemacs-config-dir minemacs-root-dir)))
            :wk "User config directory")

    ;; ====== Buffers ======
    "b"   '(nil :wk "buffer")
    "bi"  #'ibuffer
    "bu"  #'+sudo-save-buffer
    "bS"  #'save-some-buffers
    "bs"  #'scratch-buffer
    "bM"  #'view-echo-area-messages
    "bA"  #'kill-some-buffers
    "bk"  `(,(+cmdfy! (kill-buffer (current-buffer)))
            :wk "Kill this buffer")
    "bK"  `(,(+cmdfy! (+kill-buffer-and-its-windows (current-buffer)))
            :wk "Kill this buffer and its windows")
    "bN"  '(evil-buffer-new :wk "New buffer")
    "br"  '(revert-buffer :wk "Revert")
    "bR"  '(rename-buffer :wk "Rename")
    ;; Bookmarks
    "bm"  '(nil :wk "bookmark")
    "bmm"  #'bookmark-set
    "bmd"  #'bookmark-delete
    ;; Files / Local variables
    "bv"  '(nil :wk "locals")
    "bvv" '(add-file-local-variable :wk "Add")
    "bvV" '(delete-file-local-variable :wk "Delete")
    "bvp" '(add-file-local-variable-prop-line :wk "Add in prop line")
    "bvP" '(delete-file-local-variable-prop-line :wk "Delete from prop line")
    "bvd" '(add-dir-local-variable :wk "Add to dir-locals")
    "bvD" '(delete-dir-local-variable :wk "Delete from dir-locals")
    "bvr"  '(nil :wk "reload dir-locals for...")
    "bvrr" '(+dir-locals-reload-for-this-buffer :wk "This buffer")
    "bvrd" '(+dir-locals-reload-for-all-buffers-in-this-directory :wk "All buffers in this directory")

    ;; ====== Insert ======
    "i"   '(nil :wk "insert")
    "iu"  '(insert-char :wk "Unicode char")
    "ie"  `(,(when (>= emacs-major-version 29) #'emoji-search) :wk "Emoji")

    ;; ====== Window ======
    "w"   '(nil :wk "window")
    "ww"  '(evil-window-next :wk "Next")
    "wW"  '(evil-window-prev :wk "Previous")
    "ws"  '(evil-window-split :wk "Split")
    "wv"  '(evil-window-vsplit :wk "Vertical split")
    "wr"  '(evil-window-rotate-upwards :wk "Rotate upwards")
    "wR"  '(evil-window-rotate-downwards :wk "Rotate downwards")
    "w+"  '(evil-window-increase-width :wk "Increase width")
    "w-"  '(evil-window-decrease-width :wk "Decrease width")
    "wd"  #'delete-window
    "wD"  #'delete-window-on
    "wm"  #'maximize-window
    "wu"  #'winner-undo
    "wU"  #'winner-redo

    ;; ====== Applications (Open) ======
    "o"   '(nil   :wk "app/open")
    "o-"  '(dired :wk "Dired") ;; Will be overwritten if dirvish is used
    "oe"  #'eshell

    ;; ====== Search ======
    "s"   '(nil :wk "search")

    ;; ======  Mode specific a.k.a. "local leader" ======
    "m"   '(nil :wk "mode-specific")

    ;; ====== VC ======
    "g"   '(nil :wk "git/vc")

    ;; ====== Toggle ======
    "t"   '(nil :wk "toggle")
    "td"  '(toggle-debug-on-error :wk "Debug on error")
    "tr"  #'read-only-mode
    "tl"  #'follow-mode
    "tM"  '(+messages-auto-tail-toggle :wk "Auto-tail *Messages*")
    "tV"  '(netextender-toggle :wk "NetExtender")

    ;; ====== Code ======
    "c"   '(nil :wk "code")
    "cf"  '(nil :wk "format buffer")

    ;; ====== Workspaces ======
    "r"   '(nil :wk "workspace") ;; TODO (use tab-bar-mode and tab-line-mode)

    ;; ====== Notes ======
    "n"   '(nil :wk "notes")

    ;; ====== Help ======
    "h"   '(nil :wk "help")
    "hi"  #'info
    "he"  '(nil :wk "elisp/emacs")
    "hes" #'elisp-index-search
    "hem" #'info-emacs-manual
    "hei" #'Info-search
    "hd"  '(nil :wk "describe")
    "hdk" #'describe-key
    "hdm" #'describe-keymap
    "hdb" #'describe-bindings
    "hds" #'describe-symbol
    "hdv" #'describe-variable
    "hdc" #'describe-command
    "hdf" #'describe-function
    "hdp" #'describe-package

    ;; ====== Project ======
    "p"   '(nil :wk "project")))

(use-package hydra
  :straight t
  :defer t)


(provide 'me-keybindings)

;;; keybindings.el ends here
