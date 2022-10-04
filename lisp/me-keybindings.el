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
  (which-key-idle-delay          0.3)
  (which-key-prefix-prefix       "↪ ")
  (which-key-sort-order          'which-key-key-order-alpha)
  (which-key-min-display-lines   6)
  (which-key-max-display-columns nil)
  :config
  (which-key-mode)
  (which-key-setup-minibuffer))

(use-package hydra
  :straight t
  :defer t)

(use-package use-package-hydra
  :after hydra
  :straight t)

;;; General.el
(use-package general
  :straight t
  :config
  ;; Global leader
  (general-create-definer me-global-def
    :states '(normal motion visual)
    :keymaps 'override
    :prefix "SPC")

  ;; Local leader
  (general-create-definer me-local-def
    :states '(normal motion visual)
    :prefix "SPC m")

  ;; Local leader
  (general-create-definer me-map-def
    :states '(normal motion visual))

  (me-global-def
    ;; Top level functions
    "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
    "SPC" '(execute-extended-command :which-key "M-x")
    ";"   '(pp-eval-expression :which-key "Eval expression")
    ":"   '(project-find-file :which-key "Find file in project")
    "X"   '(org-capture :which-key "Org capture")
    "."   '(find-file :which-key "Find file")

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
    "fE"  `(,(me-cmdfy! (funcall (if (fboundp 'dirvish) 'dirvish 'dired) user-emacs-directory)) :which-key ".emacs.d")

    ;; Files / Local variables
    "fv"  '(nil :which-key "Local variable")
    "fvv" '(add-file-local-variable :which-key "Add")
    "fvV" '(delete-file-local-variable :which-key "Delete")
    "fvp" '(add-file-local-variable-prop-line :which-key "Add in prop line")
    "fvP" '(delete-file-local-variable-prop-line :which-key "Delete from prop line")
    "fvd" '(add-dir-local-variable :which-key "Add to dir-locals")
    "fvD" '(delete-dir-local-variable :which-key "Delete from dir-locals")
    "fvr" '(me-dir-locals-reload-for-current-buffer :which-key "Reload dir-locals for this buffer")
    "fvR" '(me-dir-locals-reload-for-all-buffers-in-this-directory :which-key "Reload dir-locals for this directory")

    ;; Buffers
    "b"   '(nil :which-key "buffer")
    "bi"  '(ibuffer :which-key "Ibuffer")
    "bu"  '(me-sudo-save-buffer :which-key "Sudo save buffer")
    "bp"  '(project-switch-to-buffer :which-key "Switch to buffer in project")
    "bk"  `(,(me-cmdfy! (kill-buffer (current-buffer))) :which-key "Kill buffer")

    ;; Insert
    "i"   '(nil :which-key "insert")
    "iu"  '(insert-char :which-key "Unicode char")

    ;; Window
    "w"   '(nil :which-key "window")
    "ww"  '(evil-window-next :which-key "Next")
    "wW"  '(evil-window-prev :which-key "Previous")
    "ws"  '(evil-window-split :which-key "Split")
    "wv"  '(evil-window-vsplit :which-key "Vertical split")
    "wr"  '(evil-window-rotate-upwards :which-key "Rotate upwards")
    "wR"  '(evil-window-rotate-downwards :which-key "Rotate downwards")
    "wd"  '(evil-window-delete :which-key "Delete")
    "w+"  '(evil-window-increase-width :which-key "Increase width")
    "w-"  '(evil-window-decrease-width :which-key "Decrease width")

    ;; Applications (Open)
    "o"   '(nil   :which-key "app/open")
    "o-"  '(dired :which-key "Dired") ;; Will be overwritten if dirvish is used

    ;; Search
    "s"   '(nil   :which-key "search")

    ;; VC
    "g"   '(nil :which-key "git/vc")

    ;; Toggle
    "t"   '(nil :which-key "toggle")

    ;; Code
    "c"   '(nil :which-key "code")

    ;; Toggle
    "p"   '(nil :which-key "project")
    "pp"  '(project-switch-project :which-key "Decrease width")
    "ps"  '(project-search :which-key "Search project")
    "pc"  '(project-compile :which-key "Compile")
    "pd"  '(project-find-dir :which-key "Find directory")
    "pf"  '(project-find-file :which-key "Find file")
    "pF"  '(nil :which-key "Forget")
    "pFz" '(project-forget-zombie-projects :which-key "Zombie projects")
    "pFp" '(project-forget-project :which-key "Project")
    "pFu" '(project-forget-projects-under :which-key "Projects under...")))


(provide 'me-keybindings)

;;; keybindings.el ends here
