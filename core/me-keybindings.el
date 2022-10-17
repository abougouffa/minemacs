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
  (general-create-definer +map
    :states '(normal motion visual)
    :keymaps 'override
    :prefix minemacs-leader-key)

  ;; Local leader
  (general-create-definer +map-local
    :states '(normal motion visual)
    :prefix minemacs-localleader-key)

  ;; Local leader
  (general-create-definer +map-key
    :states '(normal motion visual))

  (+map
    ;; Top level functions
    "TAB" '(switch-to-next-buffer :which-key "Next buffer")
    "<backtab>" '(switch-to-prev-buffer :which-key "Previous buffer")
    "SPC" '(execute-extended-command :which-key "M-x")
    ";"   '(pp-eval-expression :which-key "Eval expression")
    ":"   #'project-find-file
    "X"   #'org-capture
    "."   #'find-file
    "u"   '(universal-argument :which-key "C-u")

    ;; Quit/Session
    "q"   '(nil :which-key "quit/session")
    "qq"  #'save-buffers-kill-terminal
    "qQ"  #'kill-emacs
    "qs"  #'server-start
    "qR"  #'recover-session

    ;; Files
    "f"   '(nil               :which-key "file")
    "fS"  '(write-file        :which-key "Save as ...")
    "fD"  #'+delete-this-file
    "fu"  #'+sudo-find-file
    "fU"  #'+sudo-this-file
    "fR"  #'+move-this-file
    "ff"  #'find-file
    "fs"  #'save-buffer
    "ft"  #'recover-this-file
    "fT"  #'recover-file
    "fy"  `(,(+cmdfy! (when-let ((f (buffer-file-name)))
                       (with-temp-buffer (insert f)
                        (kill-ring-save (point-min) (point-max)))))
            :which-key "Yank file name")
    "fE"  `(,(+cmdfy! (dired (or minemacs-config-dir minemacs-root-dir)))
            :which-key "User config directory")

    ;; Buffers
    "b"   '(nil :which-key "buffer")
    "bi"  #'ibuffer
    "bu"  #'+sudo-save-buffer
    "bp"  #'project-switch-to-buffer
    "bK"  #'kill-some-buffers
    "bm"  #'bookmark-set
    "bM"  #'bookmark-delete
    "bk"  `(,(+cmdfy! (kill-buffer (current-buffer)))
            :which-key "Kill this buffer")
    "bN"  '(evil-buffer-new :which-key "New buffer")
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
    "bvr" '(+dir-locals-reload-for-this-buffer :which-key "Reload dir-locals for this buffer")
    "bvR" '(+dir-locals-reload-for-all-buffers-in-this-directory :which-key "Reload dir-locals for this directory")

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
    "tM"  '(+messages-auto-tail-toggle :which-key "Auto-tail *Messages*")
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
    "pD"  '(+dir-locals-open-or-create :which-key "Open/create dir-locals file")
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
