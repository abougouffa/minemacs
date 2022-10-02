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
  (which-key-prefix-prefix       "+")
  (which-key-sort-order          'which-key-key-order-alpha)
  (which-key-min-display-lines   6)
  (which-key-max-display-columns nil)
  :config
  (which-key-mode)
  (which-key-setup-minibuffer))

(use-package hydra
  :straight t)

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
    :prefix "SPC l")

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

    ;; Quit
    "q"   '(nil :which-key "quit")
    "qq"  '(save-buffers-kill-terminal :which-key "Quit Emacs")
    "qQ"  '(kill-emacs :which-key "Kill Emacs")
    "qs"  '(server-start :which-key "Start daemon")

    ;; Files
    "f"   '(nil                :which-key "file")
    "ff"  '(find-file          :which-key "Find file")
    "fD"  '(me-delete-this-file :which-key "Delete this file")
    "fs"  '(save-buffer        :which-key "Save")
    "fS"  '(write-file         :which-key "Save as ...")
    "fu"  '(nil                :which-key "Sudo find file") ;; TODO
    "fU"  '(nil                :which-key "Sudo this file") ;; TODO
    "fr"  '(consult-recent-file :which-key "Recent files")
    "fR"  '(me-move-this-file  :which-key "Move/rename this file")
    "fy"  '(nil  :which-key "Yank file path") ;; TODO
    "fE"  `(,(me-cmdfy! (dired user-emacs-directory)) :which-key ".emacs.d")

    ;; Files / Local variables
    "fv"  '(nil :which-key "Local variable")
    "fvv" '(add-file-local-variable :which-key "Add")
    "fvd" '(delete-file-local-variable :which-key "Delete")
    "fvV" '(add-file-local-variable-prop-line :which-key "Add in prop line")
    "fvD" '(delete-file-local-variable-prop-line :which-key "Delete from prop line")

    ;; Buffers
    "b"   '(nil                :which-key "buffer")
    "bb"  '(consult-buffer     :which-key "Switch to buffer")
    "bk"  `(,(me-cmdfy! (kill-buffer (current-buffer))) :which-key "Kill buffer")
    "bi"  '(ibuffer            :which-key "ibuffer")

    ;; Windows
    "w"   '(nil                :which-key "window")
    "ww"  '(evil-window-next   :which-key "Next")
    "wW"  '(evil-window-prev   :which-key "Previous")
    "ws"  '(evil-window-split  :which-key "Split")
    "wv"  '(evil-window-vsplit :which-key "Vertical split")
    "wr"  '(evil-window-rotate-upwards   :which-key "Rotate upwards")
    "wR"  '(evil-window-rotate-downwards :which-key "Rotate downwards")
    "wd"  '(evil-window-delete :which-key "Delete")
    "w+"  '(evil-window-increase-width :which-key "Increase width")
    "w-"  '(evil-window-decrease-width :which-key "Decrease width")

    ;; Applications (Open)
    "o"   '(nil   :which-key "open")
    "o-"  '(dired :which-key "Dired")

    ;; VC
    "g"   '(nil  :which-key "git/vc")

    ;; Code
    "c"   '(nil                :which-key "code")))

;;; keybindings.el ends here
