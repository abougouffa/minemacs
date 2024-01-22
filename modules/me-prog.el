;;; me-prog.el --- Programming stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(unless (+emacs-features-p 'tree-sitter)
  ;; Use the external `tree-sitter' module
  (+load minemacs-obsolete-modules-dir "me-tree-sitter.el")
  (+load minemacs-obsolete-modules-dir "me-cmake.el"))

(use-package treesit-auto
  :straight (:host github :repo "renzmann/treesit-auto")
  :when (+emacs-features-p 'tree-sitter)
  :hook (minemacs-after-startup . global-treesit-auto-mode)
  :hook (minemacs-build-functions . treesit-auto-install-all)
  :custom
  (treesit-auto-install 'prompt)
  :config
  (push 'nix treesit-auto-langs)
  (push (make-treesit-auto-recipe
         :lang 'nix
         :ts-mode 'nix-ts-mode
         :remap 'nix-mode
         :url "https://github.com/nix-community/tree-sitter-nix"
         :ext "\\.nix\\'")
        treesit-auto-recipe-list))

(use-package evil-textobj-tree-sitter
  :straight (:host github :repo "meain/evil-textobj-tree-sitter" :files (:defaults "queries" "treesit-queries"))
  :after evil
  :init
  ;; Require the package on the first `prog-mode' file
  (+hook-once! prog-mode-hook (with-eval-after-load 'evil (require 'evil-textobj-tree-sitter)))
  :config
  ;; Goto start of next function
  (define-key evil-normal-state-map (kbd "]f") (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "function.outer")))
  ;; Goto start of previous function
  (define-key evil-normal-state-map (kbd "[f") (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "function.outer" t)))
  ;; Goto end of next function
  (define-key evil-normal-state-map (kbd "]F") (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "function.outer" nil t)))
  ;; Goto end of previous function
  (define-key evil-normal-state-map (kbd "[F") (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "function.outer" t t))))

(use-package awk-ts-mode
  :straight t
  :when (+emacs-features-p 'tree-sitter))

(use-package html-ts-mode
  :straight (:host github :repo "mickeynp/html-ts-mode")
  :when (+emacs-features-p 'tree-sitter))

(use-package combobulate
  :straight t
  :when (+emacs-features-p 'tree-sitter)
  :hook ((python-ts-mode js-ts-mode css-ts-mode yaml-ts-mode typescript-ts-mode tsx-ts-mode) . combobulate-mode)
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
  :after minemacs-first-file
  :demand t
  :custom
  ;; Better (!) project root detection function
  (citre-project-root-function #'+citre-recursive-project-root)
  ;; Use the project root by default to create the tags file and run the indexing command
  (citre-use-project-root-when-creating-tags t)
  :init
  (defvar +citre-recursive-root-project-detection-files '(".tags/" ".repo/" ".citre_root"))
  :config
  (defun +citre-recursive-project-root ()
    "Search recursively until we find one of `+citre-recursive-root-project-detection-files'.
Fall back to the default `citre--project-root'."
    (or
     (let ((dir (buffer-file-name)))
       (catch 'root
         (while dir
           (when (cl-some #'file-exists-p (mapcar (+apply-partially-right #'expand-file-name dir) +citre-recursive-root-project-detection-files))
             (throw 'root dir))
           (setq dir (file-name-parent-directory dir)))))
     ;; Fall back to the default detection!
     (citre--project-root))))

(use-package citre-config
  :straight citre
  :after citre
  :demand t)

(use-package consult-eglot
  :straight t
  :after consult eglot
  :init
  (+map! :keymaps 'eglot-mode-map
    "cs" '(consult-eglot-symbols :wk "Symbols"))
  :config
  ;; Provide `consult-lsp' functionality from `consult-eglot', useful for
  ;; packages that relays on `consult-lsp' (like `dirvish-subtree').
  (unless (or (memq 'lsp-mode minemacs-configured-packages)
              (fboundp 'consult-lsp-file-symbols))
    (defalias 'consult-lsp-file-symbols #'consult-eglot-symbols)))

(use-package eldoc-box
  :straight t
  :hook (prog-mode . eldoc-box-hover-at-point-mode)
  :hook (eglot-managed-mode . eldoc-box-hover-at-point-mode))

(use-package compile-multi
  :straight (:files (:defaults "extensions/compile-multi-embark/*.el" "extensions/consult-compile-multi/*.el"))
  :commands +project-compile-multi
  :init
  (+map! "pC" #'+project-compile-multi)
  :config
  (defun +project-compile-multi ()
    "Like `project-compile', but uses `compile-multi'."
    (declare (interactive-only compile))
    (interactive)
    (let ((default-directory (project-root (project-current t)))
          (compilation-buffer-name-function
           (or project-compilation-buffer-name-function
               compilation-buffer-name-function)))
      (call-interactively #'compile-multi)))

  ;; Enable integration for `consult' and `embark'
  (consult-compile-multi-mode 1)
  (compile-multi-embark-mode 1))

(use-package apheleia
  :straight t
  :custom
  (apheleia-remote-algorithm 'local) ; format remote files using local formatters
  :init
  (+map! "cff" #'apheleia-format-buffer)
  :config
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

(use-package pyvenv
  :straight t)

(use-package pyenv
  :straight (:host github :repo "aiguofer/pyenv.el")
  :hook (minemacs-first-python-file . +global-pyenv-mode-maybe)
  :custom
  (pyenv-show-active-python-in-modeline nil)
  :config
  (defun +global-pyenv-mode-maybe (&optional arg)
    "Enable `pyenv-global-mode' if it can be enabled."
    (interactive (list (if current-prefix-arg (prefix-numeric-value current-prefix-arg) 'toggle)))
    (if (file-executable-p pyenv-executable)
        (global-pyenv-mode arg)
      (+log! "The %S file doesn't exist or is not executable, `pyenv' cannot be enabled." pyenv-executable))))

(use-package gitlab-ci-mode
  :straight t)

(use-package vimrc-mode
  :straight t)

(use-package rust-mode
  :straight t
  :commands
  rust-compile rust-compile-release
  rust-check rust-test
  rust-run rust-run-release rust-run-clippy
  rust-format-buffer rust-goto-format-problem
  rust-enable-format-on-save
  :init
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

(use-package makefile-executor
  :straight t
  :hook (makefile-mode . makefile-executor-mode)
  :init
  (+map!
    "pm" '(nil :wk "makefile-executor")
    "pmm" #'makefile-executor-execute-project-target
    "pml" #'makefile-executor-execute-last)
  (+map-local! :keymaps 'makefile-mode-map
    "pmt" #'makefile-executor-execute-target
    "pmb" #'makefile-executor-execute-dedicated-buffer))

(use-package web-mode
  :straight t)

(use-package python-docstring
  :straight t
  :hook ((python-mode python-ts-mode) . python-docstring-mode))

(use-package eglot-booster
  :straight (:host github :repo "jdtsmith/eglot-booster"))


(provide 'me-prog)

;;; me-prog.el ends here
