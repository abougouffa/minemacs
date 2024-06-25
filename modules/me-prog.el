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
  :hook (minemacs-lazy . global-treesit-auto-mode)
  :custom
  (treesit-auto-install 'prompt)
  :config
  ;; Add extra grammars
  ;; BUG+FIX: Remove the C++ grammar to force using v0.22.0, newer versions
  ;; cause problems with syntax highlighting in `c++-ts-mode' buffers (abougouffa/minemacs#135)
  ;; BUG+FIX: Remove the Markdown grammar to install it correctly (renzmann/treesit-auto#102)
  (let* ((extra-recipes
          (list (make-treesit-auto-recipe
                 :lang 'xml
                 :ts-mode 'xml-ts-mode
                 :remap '(nxml-mode xml-mode)
                 :url "https://github.com/tree-sitter-grammars/tree-sitter-xml"
                 :source-dir "xml/src"
                 :ext "\\.xml\\'")
                (make-treesit-auto-recipe
                 :lang 'markdown
                 :ts-mode 'markdown-ts-mode
                 :remap '(poly-markdown-mode markdown-mode)
                 :requires 'markdown-inline
                 :url "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
                 :revision "split_parser"
                 :source-dir "tree-sitter-markdown/src"
                 :ext "\\.md\\'")
                (make-treesit-auto-recipe
                 :lang 'markdown-inline
                 :ts-mode 'markdown-inline-mode ; Fake mode to make `treesit-auto' happy
                 :remap 'markdown-inline-ts-mode
                 :requires 'markdown
                 :url "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
                 :revision "split_parser"
                 :source-dir "tree-sitter-markdown-inline/src")
                (make-treesit-auto-recipe
                 :lang 'cpp
                 :ts-mode 'c++-ts-mode
                 :remap 'c++-mode
                 :requires 'c
                 :url "https://github.com/tree-sitter/tree-sitter-cpp"
                 :revision "v0.22.0"
                 :ext "\\.cpp\\'")
                (make-treesit-auto-recipe
                 :lang 'llvm
                 :ts-mode 'llvm-ts-mode
                 :remap 'llvm-mode
                 :url "https://github.com/benwilliamgraham/tree-sitter-llvm"
                 :ext "\\.ll\\'")
                (make-treesit-auto-recipe
                 :lang 'zig
                 :ts-mode 'zig-ts-mode
                 :remap 'zig-mode
                 :url "https://github.com/GrayJack/tree-sitter-zig"
                 :ext "\\.\\(zig\\|zon\\)\\'"))))
    ;; First, delete the duplicate recipes already present in the list, if any
    (cl-callf2 cl-delete-if
        (lambda (lang) (memq (treesit-auto-recipe-lang lang) (mapcar #'treesit-auto-recipe-lang extra-recipes)))
        treesit-auto-recipe-list)
    ;; Then, add the extra recipes to the list
    (cl-callf append treesit-auto-recipe-list extra-recipes)
    (setq treesit-auto-langs (mapcar #'treesit-auto-recipe-lang treesit-auto-recipe-list)))

  ;; Ensure that installed tree-sitter languages have their corresponding `x-ts-mode' added to `auto-mode-alist'
  (treesit-auto-add-to-auto-mode-alist 'all)

  ;; Create `treesit' parsers when they are available even in non-treesit modes.
  ;; This is useful for packages like `expreg' and `ts-movement'.
  ;; BUG: Adding the Elisp grammar and creating it seems to interfere with `parinfer-rust-mode'
  (defun +treesit-enable-available-grammars-on-normal-modes ()
    "Enable `treesit' parses in non-treesit modes."
    (dolist (recipe treesit-auto-recipe-list)
      (let ((lang (treesit-auto-recipe-lang recipe)))
        (unless (fboundp (treesit-auto-recipe-ts-mode recipe)) ; When the `xxx-ts-mode' is not available
          (dolist (remap-mode (ensure-list (treesit-auto-recipe-remap recipe))) ; Get the basic mode name (non-ts)
            (let ((fn-name (intern (format "+treesit--enable-on-%s-h" remap-mode)))
                  (hook-name (intern (format "%s-hook" remap-mode))))
              (defalias fn-name (lambda () ; Create the parser if the grammar fot the language is available
                                  (when (and (treesit-available-p) (treesit-language-available-p lang))
                                    (treesit-parser-create lang))))
              (add-hook hook-name fn-name)))))))

  (+treesit-enable-available-grammars-on-normal-modes))

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

(use-package consult-eglot
  :straight t
  :after consult eglot
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

(use-package reformatter
  :straight t)

(use-package apheleia
  :straight t
  :custom
  (apheleia-remote-algorithm 'local) ; format remote files using local formatters
  :config
  (add-hook
   'nxml-mode-hook
   (satch-defun +xmllint--set-indent-h ()
     (setenv "XMLLINT_INDENT" (make-string nxml-child-indent (string-to-char " ")))))
  (push '(nxml-mode . xmllint) apheleia-mode-alist))

;; for bin in $(ls $(dirname $(which clang-13))/clang-*); do ln -s $bin $HOME/.local/bin/$(basename ${bin%-13}); done
(use-package clang-format
  :straight t)

(use-package quickrun
  :straight t)

(use-package gitlab-ci-mode
  :straight t)

(use-package vimrc-mode
  :straight t)

(use-package rust-mode
  :straight t
  :custom
  (rust-mode-treesitter-derive (+emacs-features-p 'tree-sitter)))

(use-package rustic
  :straight t
  :custom
  (rustic-lsp-client 'eglot))

(use-package cargo
  :straight t)

(use-package cuda-mode
  :straight t
  :hook (cuda-mode . display-line-numbers-mode)
  :hook (cuda-mode . hs-minor-mode))

(use-package opencl-mode
  :straight t
  :mode "\\.cl\\'")

(use-package dumb-jump
  :straight t
  :after xref
  :custom
  (dumb-jump-selector 'completing-read)
  :init
  ;; Use `dumb-jump' as `xref' backend
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package hl-todo
  :straight (:host github :repo "tarsius/hl-todo")
  :hook (prog-mode . hl-todo-mode)
  :config
  (cl-callf append hl-todo-keyword-faces
    '(("BUG"   . "#ee5555")
      ("FIX"   . "#0fa050")
      ("PROJ"  . "#447f44")
      ("IDEA"  . "#0fa050")
      ("INFO"  . "#0e9030")
      ("TWEAK" . "#fe9030")
      ("PERF"  . "#e09030"))))

(use-package rainbow-mode
  :straight t)

(use-package lua-mode
  :straight t
  :custom
  (lua-indent-level 2))

(use-package fb-mode
  :straight (:host github :repo "rversteegen/fb-mode")
  :commands fb-mode
  :mode "\\.b\\(i\\|as\\)\\'")

(use-package zig-mode
  :straight t)

(use-package franca-idl
  :straight (:host github :repo "zeph1e/franca-idl.el"))

(use-package just-mode
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
  :straight t
  :hook ((c-mode c++-mode c-ts-mode c++-ts-mode python-mode python-ts-mode rust-mode rust-ts-mode sh-mode bash-ts-mode) . breadcrumb-local-mode)
  :config
  ;; Don't show the project/file name in the header by just a file icon
  (with-eval-after-load 'nerd-icons
    (advice-add
     'breadcrumb-project-crumbs :override
     (satch-defun +breadcrumb--project:override-a ()
       (concat " " (if-let ((file buffer-file-name))
                       (nerd-icons-icon-for-file file)
                     (nerd-icons-icon-for-mode major-mode)))))))

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
  :when (+emacs-features-p 'libxml2))

(use-package add-node-modules-path
  :straight t
  :hook ((js-mode js-ts-mode js2-mode) . add-node-modules-path)
  :config
  (when (executable-find "pnpm")
    (setopt add-node-modules-path-command '("pnpm bin" "pnpm bin -w"))))


(provide 'me-prog)

;;; me-prog.el ends here
