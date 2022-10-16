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
  (push '(("\\`g" . "\\`evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "ɛ·\\1"))
        which-key-replacement-alist)
  (push '(("\\`g z" . "\\`evil-mc-\\(.*\\)") . (nil . "⌶·\\1"))
        which-key-replacement-alist)
  (push '(("\\`g c" . "\\`evilnc-\\(.*\\)") . (nil . "#·\\1"))
        which-key-replacement-alist)
  (push '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "m·\\1"))
        which-key-replacement-alist)
  (which-key-mode)
  (which-key-setup-minibuffer))


;;; General.el
(use-package general
  :straight t
  :config
  ;; Global leader
  (general-create-definer me-map
    :states '(normal motion visual)
    :keymaps 'override
    :prefix me-leader-key)

  ;; Local leader
  (general-create-definer me-map-local
    :states '(normal motion visual)
    :prefix me-localleader-key)

  ;; Local leader
  (general-create-definer me-map-key
    :states '(normal motion visual))

  (me-map
    ;; Top level functions
    "TAB" '(switch-to-next-buffer :which-key "Buffer next")
    "<backtab>" '(switch-to-prev-buffer :which-key "Buffer prev")
    "SPC" '(execute-extended-command :which-key "M-x")
    ";"   '(pp-eval-expression :which-key "Eval expression")
    ":"   '(project-find-file :which-key "Find file in project")
    "X"   '(org-capture :which-key "Org capture")
    "."   '(find-file :which-key "Find file")
    "u"   '(universal-argument :which-key "Universal arg")

    ;; Quit/Session
    "q"   '(nil :which-key "quit/session")
    "qq"  '(save-buffers-kill-terminal :which-key "Quit Emacs")
    "qQ"  '(kill-emacs :which-key "Kill Emacs")
    "qs"  '(server-start :which-key "Start daemon")
    "qR"  '(recover-session :which-key "Recover session")

    ;; Files
    "f"   '(nil                 :which-key "file")
    "ff"  '(find-file           :which-key "Find file")
    "fs"  '(save-buffer         :which-key "Save")
    "fS"  '(write-file          :which-key "Save as ...")
    "fD"  '(me-delete-this-file :which-key "Delete this file")
    "fu"  '(me-sudo-find-file   :which-key "Sudo find file")
    "fU"  '(me-sudo-this-file   :which-key "Sudo this file")
    "fR"  '(me-move-this-file   :which-key "Move/rename this file")
    "fy"  '(nil                 :which-key "Yank file path") ;; TODO
    "ft"  '(recover-this-file   :which-key "Recover this file")
    "fT"  '(recover-file        :which-key "Recover file")
    "fE"  `(,(me-cmdfy! (dired (or minemacs-config-dir minemacs-root-dir))) :which-key "User config directory")

    ;; Buffers
    "b"   '(nil :which-key "buffer")
    "bi"  #'ibuffer
    "bu"  '(me-sudo-save-buffer :which-key "Sudo save buffer")
    "bp"  '(project-switch-to-buffer :which-key "Switch to buffer in project")
    "bk"  `(,(me-cmdfy! (kill-buffer (current-buffer))) :which-key "Kill buffer")
    "bK"  '(kill-some-buffers :which-key "Kill some buffers")
    "bN"  '(evil-buffer-new :which-key "New buffer")
    "bm"  #'bookmark-set
    "bM"  #'bookmark-delete
    "br"  '(revert-buffer :which-key "Revert")
    "bR"  '(rename-buffer :which-key "Rename")
    ;; Files / Local variables
    "bv"  '(nil :which-key "locals")
    "bvv" '(add-file-local-variable :which-key "Add")
    "bvV" '(delete-file-local-variable :which-key "Delete")
    "bvp" '(add-file-local-variable-prop-line :which-key "Add in prop line")
    "bvP" '(delete-file-local-variable-prop-line :which-key "Delete from prop line")
    "bvd" '(add-dir-local-variable :which-key "Add to dir-locals")
    "bvD" '(delete-dir-local-variable :which-key "Delete from dir-locals")
    "bvr" '(me-dir-locals-reload-for-current-buffer :which-key "Reload dir-locals for this buffer")
    "bvR" '(me-dir-locals-reload-for-all-buffers-in-this-directory :which-key "Reload dir-locals for this directory")

    ;; Insert
    "i"   '(nil :which-key "insert")
    "iu"  '(insert-char :which-key "Unicode char")
    "ie"  `(,(when (>= emacs-major-version 29) #'emoji-insert) :which-key "Emoji")

    ;; Window
    "w"   '(nil :which-key "window")
    "ww"  '(evil-window-next :which-key "Next")
    "wW"  '(evil-window-prev :which-key "Previous")
    "ws"  '(evil-window-split :which-key "Split")
    "wv"  '(evil-window-vsplit :which-key "Vertical split")
    "wr"  '(evil-window-rotate-upwards :which-key "Rotate upwards")
    "wR"  '(evil-window-rotate-downwards :which-key "Rotate downwards")
    "w+"  '(evil-window-increase-width :which-key "Increase width")
    "w-"  '(evil-window-decrease-width :which-key "Decrease width")
    "wd"  #'delete-window
    "wD"  #'delete-window-on
    "wm"  #'maximize-window
    "wu"  #'winner-undo
    "wU"  #'winner-redo

    ;; Applications (Open)
    "o"   '(nil   :which-key "app/open")
    "o-"  '(dired :which-key "Dired") ;; Will be overwritten if dirvish is used

    ;; Search
    "s"   '(nil :which-key "search")

    ;; Mode specific a.k.a. "local leader"
    "m"   '(nil :which-key "mode-specific")

    ;; VC
    "g"   '(nil :which-key "git/vc")

    ;; Toggle
    "t"   '(nil :which-key "toggle")
    "td"  '(toggle-debug-on-error :which-key "Debug on error")
    "tr"  #'read-only-mode
    "tl"  #'follow-mode
    "tM"  '(me-messages-auto-tail-toggle :which-key "Auto-tail *Messages*")
    "tV"  '(netextender-toggle :which-key "NetExtender")

    ;; Code
    "c"   '(nil :which-key "code")

    ;; Workspaces TODO
    "r"   '(nil :which-key "workspace") ;; TODO (using tab-bar-mode and tab-line-mode)

    ;; Notes
    "n"   '(nil :which-key "notes")

    ;; Help
    "h"   '(nil :which-key "help")

    ;; Project
    "p"   '(nil :which-key "project")
    "pp"  '(project-switch-project :which-key "Switch")
    "pc"  '(project-compile :which-key "Compile")
    "pd"  '(project-find-dir :which-key "Find directory")
    "pf"  '(project-find-file :which-key "Find file")
    "pD"  '(me-dir-locals-open-or-create :which-key "Open/create dir-locals file")
    ;; Forget
    "pF"  '(nil :which-key "Forget")
    "pFz" '(project-forget-zombie-projects :which-key "Zombie projects")
    "pFp" '(project-forget-project :which-key "Project")
    "pFu" '(project-forget-projects-under :which-key "Projects under...")
    ;; Search/replace
    "ps"  '(nil :which-key "Search/replace")
    "pss" '(project-search :which-key "Search")
    "psn" '(fileloop-continue :which-key "Next match")
    "psr" #'project-query-replace-regexp
    "psf" #'project-find-regexp))


(use-package hydra
  :straight t
  :after minemacs-loaded)


(provide 'me-keybindings)

;;; keybindings.el ends here
