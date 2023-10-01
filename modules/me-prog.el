;;; me-prog.el --- Programming stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(if (not (+emacs-features-p 'tree-sitter))
    ;; Use the external `tree-sitter' module
    (+load minemacs-modules-dir "obsolete/me-tree-sitter.el")
  (use-package treesit-auto
    :straight (:host github :repo "renzmann/treesit-auto")
    :hook (minemacs-after-startup . global-treesit-auto-mode)
    :hook (minemacs-build-functions . treesit-auto-install-all)
    :custom
    (treesit-auto-install 'prompt)
    :config
    ;; Install all languages when calling `treesit-auto-install-all'
    (setq treesit-language-source-alist (treesit-auto--build-treesit-source-alist)))

  ;; To avoid installing `tree-sitter' as the used `ts-fold' fork uses the
  ;; built-in `treesit'
  (push 'tree-sitter straight-built-in-pseudo-packages)

  (use-package ts-fold
    :straight (:host github :repo "abougouffa/ts-fold" :branch "andrew-sw/treesit-el-support")
    :after treesit treesit-auto
    :demand t
    :init
    (global-ts-fold-mode 1))

  (use-package combobulate
    :straight t
    :hook ((python-ts-mode js-ts-mode css-ts-mode yaml-ts-mode typescript-ts-mode tsx-ts-mode) . combobulate-mode)
    :custom
    (combobulate-key-prefix "C-c o")))

(use-package evil-textobj-tree-sitter
  :straight t
  :after evil treesit
  :demand t
  :config
  ;; evil-textobj-tree-sitter comes with no default keybindings,
  ;; Here is a keybindings (vaX) stolen from here:
  ;; github.com/meain/dotfiles/blob/master/emacs/.config/emacs/init.el
  (define-key
   evil-outer-text-objects-map
   "m" (cons "evil-import"
             (evil-textobj-tree-sitter-get-textobj
               "import"
               '((python-mode . [(import_statement) @import])
                 (go-mode . [(import_spec) @import])
                 (rust-mode . [(use_declaration) @import])
                 (c-mode . [(preproc_include) @import])
                 (c++-mode . [(preproc_include) @import])))))
  (define-key evil-outer-text-objects-map "f" (cons "evil-outer-function" (evil-textobj-tree-sitter-get-textobj "function.outer")))
  (define-key evil-inner-text-objects-map "f" (cons "evil-inner-function" (evil-textobj-tree-sitter-get-textobj "function.inner")))
  (define-key evil-outer-text-objects-map "c" (cons "evil-outer-class" (evil-textobj-tree-sitter-get-textobj "class.outer")))
  (define-key evil-inner-text-objects-map "c" (cons "evil-inner-class" (evil-textobj-tree-sitter-get-textobj "class.inner")))
  (define-key evil-outer-text-objects-map "n" (cons "evil-outer-comment" (evil-textobj-tree-sitter-get-textobj "comment.outer")))
  (define-key evil-inner-text-objects-map "n" (cons "evil-outer-comment" (evil-textobj-tree-sitter-get-textobj "comment.outer")))
  (define-key evil-outer-text-objects-map "o" (cons "evil-outer-loop" (evil-textobj-tree-sitter-get-textobj "loop.outer")))
  (define-key evil-inner-text-objects-map "o" (cons "evil-inner-loop" (evil-textobj-tree-sitter-get-textobj "loop.inner")))
  (define-key evil-outer-text-objects-map "v" (cons "evil-outer-conditional" (evil-textobj-tree-sitter-get-textobj "conditional.outer")))
  (define-key evil-inner-text-objects-map "v" (cons "evil-inner-conditional" (evil-textobj-tree-sitter-get-textobj "conditional.inner")))
  (define-key evil-inner-text-objects-map "a" (cons "evil-inner-parameter" (evil-textobj-tree-sitter-get-textobj "parameter.inner")))
  (define-key evil-outer-text-objects-map "a" (cons "evil-outer-parameter" (evil-textobj-tree-sitter-get-textobj "parameter.outer")))
  (define-key evil-normal-state-map (kbd "]a") (cons "goto-parameter-start" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "parameter.inner"))))
  (define-key evil-normal-state-map (kbd "[a") (cons "goto-parameter-start" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "parameter.inner" t))))
  (define-key evil-normal-state-map (kbd "]A") (cons "goto-parameter-end" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "parameter.inner" nil t))))
  (define-key evil-normal-state-map (kbd "[A") (cons "goto-parameter-end" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "parameter.inner" t t))))
  (define-key evil-normal-state-map (kbd "]v") (cons "goto-conditional-start" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "conditional.outer"))))
  (define-key evil-normal-state-map (kbd "[v") (cons "goto-conditional-start" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "conditional.outer" t))))
  (define-key evil-normal-state-map (kbd "]V") (cons "goto-conditional-end" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "conditional.outer" nil t))))
  (define-key evil-normal-state-map (kbd "[V") (cons "goto-conditional-end" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "conditional.outer" t t))))
  (define-key evil-normal-state-map (kbd "]c") (cons "goto-class-start" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "class.outer"))))
  (define-key evil-normal-state-map (kbd "[c") (cons "goto-class-start" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "class.outer" t))))
  (define-key evil-normal-state-map (kbd "]C") (cons "goto-class-end" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "class.outer" nil t))))
  (define-key evil-normal-state-map (kbd "[C") (cons "goto-class-end" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "class.outer" t t))))
  (define-key evil-normal-state-map (kbd "]n") (cons "goto-comment-start" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "comment.outer"))))
  (define-key evil-normal-state-map (kbd "[n") (cons "goto-comment-start" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "comment.outer" t))))
  (define-key evil-normal-state-map (kbd "]N") (cons "goto-comment-end" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "comment.outer" nil t))))
  (define-key evil-normal-state-map (kbd "[N") (cons "goto-comment-end" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "comment.outer" t t))))
  (define-key evil-normal-state-map (kbd "]f") (cons "goto-function-start" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "function.outer") (reposition-window))))
  (define-key evil-normal-state-map (kbd "[f") (cons "goto-function-start" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "function.outer" t) (reposition-window))))
  (define-key evil-normal-state-map (kbd "]F") (cons "goto-function-end" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "function.outer" nil t) (reposition-window))))
  (define-key evil-normal-state-map (kbd "[F") (cons "goto-function-end" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "function.outer" t t) (reposition-window)))))

(use-package consult-eglot
  :straight t
  :after consult eglot
  :init
  (+map! :keymaps 'eglot-mode-map
    "cs" '(consult-eglot-symbols :wk "Symbols"))
  :config
  ;; Provide `consult-lsp' functionality from `consult-eglot', useful for
  ;; packages that relays on `consult-lsp' (like `dirvish-subtree').
  (unless (or (memq 'me-lsp minemacs-modules)
              (fboundp 'consult-lsp-file-symbols))
    (defalias 'consult-lsp-file-symbols #'consult-eglot-symbols)))

(use-package eldoc-box
  :straight t
  :hook (prog-mode . eldoc-box-hover-at-point-mode)
  :hook (eglot-managed-mode . eldoc-box-hover-at-point-mode))

(use-package compile-multi
  :straight t
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
      (call-interactively #'compile-multi))))

(use-package apheleia
  :straight t
  :init
  (+map! "cff" #'apheleia-format-buffer)
  :config
  (dolist (alist '((lisp-data-mode . lisp-indent)
                   (emacs-lisp-mode . lisp-indent)
                   (sh-mode . shfmt)))
    (add-to-list 'apheleia-mode-alist alist)))

(use-package editorconfig
  :straight t
  :hook (prog-mode . editorconfig-mode)
  :init
  (+map!
    "fc" '(editorconfig-find-current-editorconfig :wk "Find current EditorConfig")
    "cfe" #'editorconfig-format-buffer))

(use-package clang-format
  :straight t
  :init
  (+map! :keymaps '(c-mode-map c++-mode-map c-ts-mode-map c++-ts-mode-map
                    cuda-mode-map scad-mode-map)
    "cfc" #'clang-format-buffer))

;;; Modes
(use-package vimrc-mode
  :straight t
  :mode "\\.vim\\(rc\\)?\\'")

(unless (+emacs-features-p 'tree-sitter)
  (+load minemacs-modules-dir "obsolete/me-cmake.el"))

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
         '(("BUG" . "#ee5555")
           ("PROJ" . "#447f44")
           ("IDEA" . "#0fa050")
           ("INFO" . "#0e9030")
           ("TWEAK" . "#fe9030")
           ("PERF" . "#e09030")))))

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

(use-package makefile-executor
  :straight t
  :hook (makefile-mode . makefile-executor-mode))


(provide 'me-prog)

;;; me-prog.el ends here
