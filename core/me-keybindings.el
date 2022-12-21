;;; keybindings.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Abdelhak Bougouffa
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:


;; Which key
(use-package which-key
  :straight t
  :custom
  (which-key-idle-delay 0.3)
  (which-key-prefix-prefix "↪ ")
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-min-display-lines 5)
  (which-key-max-display-columns nil)
  (which-key-allow-multiple-replacements t)
  :config
  ;; g, [, ]
  (push '(("\\`[][g]" . "\\`evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "ɛ·\\1"))
        which-key-replacement-alist)
  ;; g r
  (push '(("\\`g r" . "\\`evil-mc-\\(.*\\)") . (nil . "⌶·\\1"))
        which-key-replacement-alist)
  ;; g c
  (push '(("\\`g c" . "\\`evilnc-\\(.*\\)") . (nil . "#·\\1"))
        which-key-replacement-alist)
  ;; g s
  (push '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "m·\\1"))
        which-key-replacement-alist)
  (which-key-mode)
  (which-key-setup-minibuffer))

;;; General.el
(use-package general
  :straight t
  :after which-key
  :config
  (general-auto-unbind-keys)

  ;; Global leader
  (general-create-definer +map
    :states '(normal visual insert emacs)
    :keymaps 'override
    :prefix minemacs-leader-key
    :global-prefix minemacs-global-leader-prefix)

  ;; Local leader
  (general-create-definer +map-local
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix minemacs-localleader-key
    :global-prefix minemacs-global-mode-prefix)

  ;; Map a key in normal, motion and visual states
  (general-create-definer +map-key
    :states '(normal visual))

  (+map
    ;; ====== Top level functions ======
    "TAB" '(switch-to-next-buffer :wk "Next buffer")
    "<backtab>" '(switch-to-prev-buffer :wk "Previous buffer")
    "SPC" '(execute-extended-command :wk "M-x")
    ";"   '(pp-eval-expression :wk "Eval expression")
    "X"   #'org-capture
    "."   #'find-file
    "u"   '(universal-argument :wk "C-u")

    ;; ====== Quit/Session ======
    "q"   '(nil :wk "quit/session")
    "qq"  #'save-buffers-kill-terminal
    "qQ"  #'kill-emacs
    "qs"  #'server-start
    "qR"  #'recover-session

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
    "bA"  #'kill-some-buffers
    "bm"  #'bookmark-set
    "bM"  #'bookmark-delete
    "bk"  `(,(+cmdfy! (kill-buffer (current-buffer)))
            :wk "Kill this buffer")
    "bK"  `(,(+cmdfy! (+kill-buffer-and-its-windows (current-buffer)))
            :wk "Kill this buffer and its windows")
    "bN"  '(evil-buffer-new :wk "New buffer")
    "br"  '(revert-buffer :wk "Revert")
    "bR"  '(rename-buffer :wk "Rename")
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

    ;; ====== Project ======
    "p"   '(nil :wk "project")))

(use-package hydra
  :straight t
  :after minemacs-loaded)


(provide 'me-keybindings)

;;; keybindings.el ends here
