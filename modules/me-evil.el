;; me-evil.el --- Emacs as Vim! -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

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
  :unless (+package-disabled-p 'evil 'me-evil)
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
      (evil-collection-define-key 'normal 'emacs-lisp-mode-map
        "gr" 'xref-find-references))))

(use-package evil-snipe
  :straight t
  :unless (+package-disabled-p 'evil 'me-evil)
  :hook (evil-mode . evil-snipe-mode)
  :hook (evil-mode . evil-snipe-override-mode)
  :custom
  (evil-snipe-scope 'buffer)
  (evil-snipe-repeat-scope 'buffer)
  (evil-snipe-smart-case t)
  (evil-snipe-auto-scroll t))

(use-package evil-numbers
  :straight t
  :unless (+package-disabled-p 'evil 'me-evil)
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
  :unless (+package-disabled-p 'evil 'me-evil)
  :commands evilnc-comment-operator evilnc-copy-and-comment-operator
  :init
  (+nvmap!
    "gc" #'evilnc-comment-operator
    "gC" #'evilnc-copy-and-comment-operator))

(cl-defmacro +evil-conf-for! (package module &optional &key init-form &key config-form)
  (declare (indent 2))
  `(when (and (not (+package-disabled-p ',package ',module))
          (memq ',module (append minemacs-core-modules minemacs-modules)))
    ,init-form
    ,(when config-form
      `(with-eval-after-load ',package ,config-form))))



;;; For `me-keybindings'

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
    "ie"   (when (>= emacs-major-version 29) #'emoji-search)

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
  :commands evil-iedit-state/iedit-mode
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



;;; For `me-natural-langs'

(when (memq 'me-natural-langs minemacs-modules)
  (+nvmap! "z=" #'+spellcheck-correct))



;;; For `me-editor'

(when (memq 'me-editor minemacs-modules)
  ;; Bind `+yank-region-as-paragraph' (autoloaded from "me-lib.el")
  (+nvmap! "gy" #'+kill-region-as-paragraph))



;;; For `me-extra'

(+evil-conf-for! better-jumper me-extra
  :init-form
  (progn
   (keymap-global-set "<remap> <evil-jump-forward>" #'better-jumper-jump-forward)
   (keymap-global-set "<remap> <evil-jump-backward>" #'better-jumper-jump-backward)
   (keymap-global-set "<remap> <xref-pop-marker-stack>" #'better-jumper-jump-backward)))



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



;;; For `me-prog'

(use-package evil-textobj-tree-sitter
  :straight (:host github :repo "meain/evil-textobj-tree-sitter" :files (:defaults "queries" "treesit-queries"))
  :when (memq 'me-prog minemacs-modules)
  :after evil minemacs-first-file
  :init
  ;; Require the package on the first `prog-mode' file
  (+hook-once! prog-mode-hook (require 'evil-textobj-tree-sitter))
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



;;; For `me-files'

(+evil-conf-for! dirvish me-files
  :config-form
  (+nvmap! :keymaps 'dirvish-mode-map
    "q" #'dirvish-quit
    "s" #'dirvish-subtree-toggle
    "y" #'dirvish-yank-menu))



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



;;; For `me-docs'

(+evil-conf-for! nov me-docs
  :config-form
  (+nmap! :keymaps 'nov-mode-map "RET" #'nov-scroll-up))



;;; For `me-docs'

(+evil-conf-for! elfeed me-rss
  :config-form
  (progn
    (+nmap! :keymaps 'elfeed-search-mode-map "d" #'+elfeed-youtube-dl)
    (+nmap! :keymaps 'elfeed-show-mode-map "D" #'+elfeed-download-image)))



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



;;; For `me-vc'

(+evil-conf-for! git-commit me-vc
  :config-form
  (with-eval-after-load 'evil (evil-set-initial-state 'git-commit-mode 'insert)))



;;; For `me-vc'

(+evil-conf-for! diffview me-vc
  :config-form
  (+nvmap! :keymaps 'diffview--mode-map
    "="   #'diffview--align-windows
    "+"   #'diffview--align-windows
    "C-j" #'diffview--next-file
    "C-k" #'diffview--prev-file
    "q"   #'diffview--quit))



;;; For `me-ui'

(+evil-conf-for! pulsar me-ui
  :config-form
  (with-eval-after-load 'evil
    (cl-callf append pulsar-pulse-functions
      '(evil-yank evil-paste-after evil-paste-before
        evil-delete evil-delete-line evil-delete-whole-line
        evil-goto-last-change evil-goto-last-change-reverse))))


(provide 'me-evil)

;;; me-evil.el ends here
