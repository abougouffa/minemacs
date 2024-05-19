;;; me-prog.el --- Programming stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(unless (+emacs-features-p 'tree-sitter)
  ;; Use the external `tree-sitter' module
  (+load minemacs-obsolete-modules-dir "me-tree-sitter.el"))

(use-package treesit-auto
  :straight (:host github :repo "renzmann/treesit-auto")
  :when (+emacs-features-p 'tree-sitter)
  :hook (minemacs-build-functions . treesit-auto-install-all)
  :commands global-treesit-auto-mode
  :custom
  (treesit-auto-install 'prompt)
  :init
  (+hook-once! prog-mode-hook
    (global-treesit-auto-mode 1)
    (+treesit-enable-available-grammars-on-normal-modes))
  :config
  (let ((extra-recipes (list (make-treesit-auto-recipe
                              :lang 'xml
                              :ts-mode 'xml-ts-mode
                              :remap '(nxml-mode xml-mode)
                              :url "https://github.com/ObserverOfTime/tree-sitter-xml"
                              :source-dir "tree-sitter-xml/src"
                              :ext "\\.xml\\'")
                             (make-treesit-auto-recipe
                              :lang 'llvm
                              :ts-mode 'llvm-ts-mode
                              :remap 'llvm-mode
                              :url "https://github.com/benwilliamgraham/tree-sitter-llvm"
                              :ext "\\.ll\\'")
                             (make-treesit-auto-recipe
                              :lang 'elisp
                              :ts-mode 'emacs-lisp-ts-mode
                              :remap 'emacs-lisp-mode
                              :url "https://github.com/Wilfred/tree-sitter-elisp"
                              :ext "\\.el\\'"))))
    (setq treesit-auto-langs (append treesit-auto-langs (mapcar #'treesit-auto-recipe-lang extra-recipes))
          treesit-auto-recipe-list (append treesit-auto-recipe-list extra-recipes)))

  ;; Ensure that installed tree-sitter languages have their corresponding `x-ts-mode' added to `auto-mode-alist'
  (treesit-auto-add-to-auto-mode-alist 'all)

  ;; Make `treesit' parsers even in non-treesit modes, useful for packages like `expreg' and `ts-movement'
  (defun +treesit-enable-available-grammars-on-normal-modes ()
    (dolist (recipe treesit-auto-recipe-list)
      (let ((lang (treesit-auto-recipe-lang recipe)))
        (unless (fboundp (treesit-auto-recipe-ts-mode recipe)) ;; When the `xxx-ts-mode' is not available
          (dolist (remap-mode (ensure-list (treesit-auto-recipe-remap recipe)))
            (add-hook (intern (format "%s-hook" remap-mode))
                      (defalias (intern (format "+treesit--enable-on-%s-h" remap-mode))
                        (lambda ()
                          (when (and (treesit-available-p) (treesit-language-available-p lang))
                            (treesit-parser-create lang)))))))))))

(use-package evil-textobj-tree-sitter
  :straight (:host github :repo "meain/evil-textobj-tree-sitter" :files (:defaults "queries" "treesit-queries"))
  :unless (+package-disabled-p 'evil 'me-evil)
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

(when (+emacs-features-p 'tree-sitter)
  (push 'treesit straight-built-in-pseudo-packages)) ; ts-movement depends on it

(use-package ts-movement
  :straight (:host github :repo "haritkapadia/ts-movement")
  :when (+emacs-features-p 'tree-sitter)
  :hook ((prog-mode conf-mode) . +ts-movement-maybe)
  :init
  (defun +ts-movement-maybe ()
    "Enable `ts-movement-mode' when if `major-mode' is a trees-sitter mode."
    (run-with-timer 1.0 nil (lambda () (when (treesit-parser-list) (ts-movement-mode 1)))))
  :config
  (+map-local! :keymaps 'ts-movement-map "v" #'+ts-movement-transient)
  (transient-define-prefix +ts-movement-transient ()
    "Transient for ts-movement."
    [[("d" "delete-overlay-at-point" tsm/delete-overlay-at-point :transient t)
      ("D" "clear-overlays-of-type" tsm/clear-overlays-of-type :transient t)
      ("C-b" "backward-overlay" tsm/backward-overlay :transient t)
      ("C-f" "forward-overlay" tsm/forward-overlay :transient t)
      ("c" "tsm/mc/mark-all-overlays" tsm/mc/mark-all-overlays :transient t)]
     [("a" "node-start" tsm/node-start :transient t)
      ("e" "node-end" tsm/node-end :transient t)
      ("b" "node-prev" tsm/node-prev :transient t)
      ("f" "node-next" tsm/node-next :transient t)]
     [("p" "node-parent" tsm/node-parent :transient t)
      ("n" "node-child" tsm/node-child :transient t)
      ("N" "node-children" tsm/node-children :transient t)
      ("s" "node-children-of-type" tsm/node-children-of-type :transient t)
      ("m" "node-mark" tsm/node-mark :transient t)]]
    [("Q" "Quit" ignore :transient t)]))

(use-package awk-ts-mode
  :straight t
  :when (+emacs-features-p 'tree-sitter))

(use-package html-ts-mode
  :straight (:host github :repo "mickeynp/html-ts-mode")
  :when (+emacs-features-p 'tree-sitter))

(use-package combobulate
  :straight (:host github :repo "mickeynp/combobulate" :nonrecursive t) ; Cloning the `html-ts-mode' submodule causes problems
  :when (and (not os/win) (+emacs-features-p 'tree-sitter)) ; TEMP: disable on Windows
  :hook ((python-ts-mode js-ts-mode css-ts-mode yaml-ts-mode typescript-ts-mode tsx-ts-mode html-ts-mode) . combobulate-mode)
  :custom
  (combobulate-key-prefix "C-c o")
  :config
  ;; The "M-<up/down/left/right>" keys are used globally by `drag-stuff', lets
  ;; unset them for `combobulate' and use "M-S-<up/down/left/right>" instead.
  (mapc (lambda (k) (keymap-unset combobulate-key-map k 'remove)) '("M-<up>" "M-<down>" "M-<left>" "M-<right>"))
  (keymap-set combobulate-key-map "M-S-<up>" #'combobulate-splice-up)
  (keymap-set combobulate-key-map "M-S-<down>" #'combobulate-splice-down)
  (keymap-set combobulate-key-map "M-S-<left>" #'combobulate-yeet-forward)
  (keymap-set combobulate-key-map "M-S-<down>" #'combobulate-yoink-forward))

(use-package citre
  :straight t
  :after minemacs-first-c/c++-file
  :demand t
  :custom
  ;; Better (!) project root detection function
  (citre-project-root-function #'+citre-recursive-project-root)
  :init
  (defcustom +citre-recursive-root-project-detection-files '(".tags/" ".repo/" ".citre_root")
    "A list of files/directories to use as a project root markers."
    :type '(repeat string)
    :group 'minemacs-prog)

  (defcustom +citre-gtags-recursive-files-list t
    "Find files to index recursively."
    :type 'boolean
    :group 'minemacs-prog)

  (defcustom +citre-gtags-files-list-suffixes '("*.[chly]" "*.[ch]xx" "*.[ch]pp" "*.[ch]++" "*.cc" "*.hh")
    "List of filename suffixes globs to index (for extensions for example)."
    :type '(repeat string)
    :group 'minemacs-prog)

  (defcustom +citre-gtags-files-list-ignored-directories '("CVS" "RCS" "SCCS" ".git" ".hg" ".bzr" ".cdv" ".pc" ".svn" ".repo" "_MTN" "_darcs" "_sgbak" "debian")
    "List of directories to be ignored when creating the file list using `+citre-gtags-find-files-command'."
    :type '(repeat string)
    :group 'minemacs-prog)
  :config
  (defun +citre-recursive-project-root ()
    "Search recursively until we find one of `+citre-recursive-root-project-detection-files'.
Fall back to the default `citre--project-root'."
    (or (+directory-root-containing-file +citre-recursive-root-project-detection-files)
        (citre--project-root))) ; Fall back to the default detection!

  (defun +citre-gtags-find-files-command (&optional dir)
    (let* ((default-directory (or dir default-directory)))
      (concat
       "echo 'Creating list of files to index ...'\n"
       (find-cmd
        (unless +citre-gtags-recursive-files-list '(maxdepth "1"))
        `(prune (and (type "d") (name ,@+citre-gtags-files-list-ignored-directories)))
        `(iname ,@+citre-gtags-files-list-suffixes)
        '(type "f" "l")
        '(print))
       " > gtags.files\n"
       "echo 'Creating list of files to index ... done'\n")))

  (defun +citre-gtags-create-list-of-files-to-index (top-directory)
    "Create a list of files to index in TOP-DIRECTORY."
    (interactive "DCreate file list in directory: ")
    (let* ((default-directory top-directory))
      (start-process-shell-command "+citre-gtags-files-list" "*+citre-gtags-files-list*" (+citre-gtags-find-files-command)))))

(use-package citre-config
  :straight citre
  :after citre
  :demand t)

(use-package xcscope
  :straight t
  :unless os/win
  :commands cscope-create-list-of-files-to-index cscope-index-files)

(use-package clink
  :straight (:host github :repo "abougouffa/clink.el")
  :when (+emacs-features-p 'sqlite3)
  :hook (minemacs-first-c/c++-file . global-clink-mode))

(use-package rtags
  :straight t)

(use-package rtags-xref
  :straight t
  :commands rtags-xref-enable)

(use-package rscope
  :straight (:host github :repo "rjarzmik/rscope")
  :commands rscope-init rscope-regenerate-database)

(use-package eopengrok
  :straight t
  :commands
  eopengrok-mode eopengrok-find-reference eopengrok-find-text eopengrok-find-definition eopengrok-find-custom
  eopengrok-jump-to-source eopengrok-create-index eopengrok-create-index-with-enable-projects
  :config
  (+nmap! :keymaps 'eopengrok-mode-map
    "n" #'eopengrok-next-line
    "p" #'eopengrok-previous-line
    "q" #'eopengrok-quit
    "RET" #'eopengrok-jump-to-source))

(use-package srefactor
  :straight t)

(use-package consult-eglot
  :straight t
  :after consult eglot
  :init
  (+map! :keymaps 'eglot-mode-map
    "cs" '(consult-eglot-symbols :wk "Symbols"))
  :config
  ;; Provide `consult-lsp' functionality from `consult-eglot', useful for
  ;; packages that relays on `consult-lsp' (like `dirvish-subtree').
  (unless (or (not (+package-disabled-p 'lsp-mode 'obsolete/me-lsp)) (fboundp 'consult-lsp-file-symbols))
    (defalias 'consult-lsp-file-symbols #'consult-eglot-symbols)))

(use-package eldoc-box
  :straight t
  :hook (prog-mode . +eldoc-box-hover-at-point-mode-maybe)
  :hook (eglot-managed-mode . +eldoc-box-hover-at-point-mode-maybe)
  :init
  (defun +eldoc-box-hover-at-point-mode-maybe (&optional arg)
    (when (display-graphic-p)
      (eldoc-box-hover-at-point-mode arg))))

(use-package apheleia
  :straight t
  :custom
  (apheleia-remote-algorithm 'local) ; format remote files using local formatters
  :init
  (+map! "cff" #'apheleia-format-buffer)
  (defvar +xmllint-indent "    ")
  :config
  (add-hook 'nxml-mode-hook (defun +xmllint--set-indent-h () (setenv "XMLLINT_INDENT" +xmllint-indent)))
  (push '(nxml-mode . xmllint) apheleia-mode-alist))

(use-package editorconfig
  :straight t
  :hook (minemacs-first-file . editorconfig-mode)
  :init
  (+map!
    "fc" '(editorconfig-find-current-editorconfig :wk "Find current EditorConfig")
    "cfe" #'editorconfig-format-buffer)
  :config
  ;; Exclude compressed files
  (push "\\.\\(zip\\|epub\\|\\(doc\\|xls\\|ppt\\)x\\)\\'" editorconfig-exclude-regexps))

;; for bin in $(ls $(dirname $(which clang-13))/clang-*); do ln -s $bin $HOME/.local/bin/$(basename ${bin%-13}); done
(use-package clang-format
  :straight t
  :init
  (+map! :keymaps '(c-mode-map c++-mode-map c-ts-mode-map c++-ts-mode-map cuda-mode-map scad-mode-map)
    "cfc" #'clang-format-buffer))

(use-package quickrun
  :straight t
  :init
  (+map!
    "cq"  '(nil :wk "quickrun")
    "cqq" #'quickrun
    "cqQ" #'quickrun-select
    "cqs" #'quickrun-shell
    "cqa" #'quickrun-with-arg
    "cqc" #'quickrun-compile-only
    "cqC" #'quickrun-compile-only-select
    "cqd" #'quickrun-select-default))

(use-package gitlab-ci-mode
  :straight t)

(use-package vimrc-mode
  :straight t)

(use-package rust-mode
  :straight t
  :custom
  (rust-mode-treesitter-derive (+emacs-features-p 'tree-sitter))
  :config
  (+map-local! :keymaps '(rust-mode-map rust-ts-mode-map)
    "c" #'rust-compile
    "C" #'rust-compile-release
    "k" #'rust-check
    "t" #'rust-test
    "r" #'rust-run
    "R" #'rust-run-release
    "y" #'rust-run-clippy
    "f" #'rust-format-buffer
    "F" #'rust-goto-format-problem
    "S" #'rust-enable-format-on-save))

(use-package cuda-mode
  :straight t
  :hook (cuda-mode . display-line-numbers-mode)
  :hook (cuda-mode . hs-minor-mode))

(use-package opencl-mode
  :straight t
  :mode "\\.cl\\'")

(use-package dumb-jump
  :straight t
  :commands +dumb-jump-hydra/body
  :custom
  (dumb-jump-selector 'completing-read)
  :init
  (+map!
    "cj" '(+dumb-jump-hydra/body :wk "+dumb-jump-hydra"))
  ;; Use as xref backend
  (with-eval-after-load 'xref
    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))
  :config
  ;; Define Hydra keybinding (from the repo's examples)
  (defhydra +dumb-jump-hydra (:color blue :hint nil :foreign-keys warn)
    "
[Dumb Jump]                                                                         [_q_] quit
  ├─────────────────────────────────────────────────────────────────────────────────────────╮
  │  [_j_] Go          [_o_] Go other window    [_e_] Go external   [_x_] Go external other window  │
  │  [_i_] Go prompt   [_l_] Quici look         [_b_] Back                                        │
  ╰─────────────────────────────────────────────────────────────────────────────────────────╯
"
    ("j" dumb-jump-go)
    ("o" dumb-jump-go-other-window)
    ("e" dumb-jump-go-prefer-external)
    ("x" dumb-jump-go-prefer-external-other-window)
    ("i" dumb-jump-go-prompt)
    ("l" dumb-jump-quick-look)
    ("b" dumb-jump-back)
    ("q" nil :color blue)))

(use-package hl-todo
  :straight (:host github :repo "tarsius/hl-todo")
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
        (append
         hl-todo-keyword-faces
         '(("BUG"   . "#ee5555")
           ("FIX"   . "#0fa050")
           ("PROJ"  . "#447f44")
           ("IDEA"  . "#0fa050")
           ("INFO"  . "#0e9030")
           ("TWEAK" . "#fe9030")
           ("PERF"  . "#e09030")))))

(use-package rainbow-mode
  :straight t
  :init
  (+map! :keymaps '(prog-mode-map conf-mode-map text-mode-map)
    "tR" #'rainbow-mode))

(use-package lua-mode
  :straight t
  :custom
  (lua-indent-level 2))

(use-package fb-mode
  :straight (:host github :repo "rversteegen/fb-mode")
  :commands fb-mode
  :mode "\\.b\\(i\\|as\\)\\'")

(use-package franca-idl
  :straight (:host github :repo "zeph1e/franca-idl.el"))

(use-package just-mode
  :straight t)

(use-package justl
  :straight t)

(use-package cmake-mode
  :straight (:host github :repo "emacsmirror/cmake-mode" :files (:defaults "*")))

(use-package cmake-font-lock
  :straight (:host github :repo "Lindydancer/cmake-font-lock" :files (:defaults "*"))
  :custom
  (cmake-font-lock-modes '(cmake-mode cmake-ts-mode)))

(use-package web-mode
  :straight t)

(use-package python-docstring
  :straight t
  :hook ((python-mode python-ts-mode) . python-docstring-mode))

(use-package eglot-booster
  :straight (:host github :repo "jdtsmith/eglot-booster"))

(use-package breadcrumb
  :straight t)

(use-package protobuf-mode
  :straight t)

(use-package protobuf-ts-mode
  :straight (:host github :repo "emacsattic/protobuf-ts-mode")
  :when (+emacs-features-p 'tree-sitter))

(use-package llvm-ts-mode
  :straight t
  :when (+emacs-features-p 'tree-sitter))

(use-package devdocs
  :straight t
  :when (+emacs-features-p 'libxml2)
  :init
  (+map!
    "hhh" #'devdocs-lookup
    "hhp" #'devdocs-peruse
    "hhs" #'devdocs-search
    "hhI" #'devdocs-install))


(provide 'me-prog)

;;; me-prog.el ends here
