;;; me-prog.el --- Programming stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>

(use-package treesit-langs
  :straight (:host github :repo "kiennq/treesit-langs" :files (:defaults "queries"))
  :when (+emacs-features-p 'tree-sitter)
  :hook (prog-mode . +treesit-hl-enable-maybe)
  :defines +treesit-hl-enable-maybe
  :preface
  (+fn-inhibit-messages! treesit-langs-install-grammars)
  :init
  (defun +treesit-hl-enable-maybe ()
    (unless (cl-some
             #'derived-mode-p
             '(emacs-lisp-mode
               org-mode))
      (ignore-errors (treesit-hl-enable))))
  :config
  (advice-add
   'treesit-langs--hl-query-path :around
   (defun +treesit-langs--fallback-to-repos-a (old-fn lang-symbol &optional mode)
     (let ((path (apply old-fn (list lang-symbol mode))))
       (if (not (file-exists-p path))
           (concat (treesit-langs--repos-dir)
                   (format "%s/queries/" lang-symbol)
                   (file-name-nondirectory path))
         path)))))

(unless (+emacs-features-p 'tree-sitter)
  (load (concat minemacs-modules-dir "obsolete/me-tree-sitter.el")
        nil (not minemacs-verbose)))

(use-package treesit
  :straight (:type built-in)
  :when (+emacs-features-p 'tree-sitter)
  :defer t
  :config
  (setq-default treesit-font-lock-level 4))

(use-package hideif
  :straight (:type built-in)
  :init
  ;; If me-lsp is used, lsp-semantic-tokens should do a better job
  (unless (memq 'me-lsp minemacs-modules)
    (dolist (h '(c++-mode-hook c++-ts-mode-hook c-mode-hook c-ts-mode-hook cuda-mode-hook))
      (add-hook h #'hide-ifdef-mode)))
  :defer t
  :custom
  (hide-ifdef-shadow t)
  (hide-ifdef-initially t))

(use-package eglot
  :straight `(:type ,(if (< emacs-major-version 29) 'git 'built-in))
  :commands +eglot-auto-enable
  :general
  (+map
    :infix "c"
    "e"  '(nil :wk "eglot session")
    "ee" #'eglot
    "eA" #'+eglot-auto-enable)
  :custom
  (eglot-autoshutdown t) ;; shutdown after closing the last managed buffer
  (eglot-sync-connect 0) ;; async, do not block
  (eglot-extend-to-xref t) ;; can be interesting!
  :config
  (defun +eglot-auto-enable ()
    (interactive)
    (dolist (h '(c++-mode-hook c++-ts-mode-hook
                 c-mode-hook c-ts-mode-hook
                 python-mode-hook python-ts-mode-hook
                 rust-mode-hook cmake-mode-hook))
      (add-hook h #'eglot-ensure)
      (remove-hook h #'lsp-deferred)))

  (+map :keymaps 'eglot-mode-map
    :infix "c"
    "fF" #'eglot-format-buffer
    "d"  '(eglot-find-declaration :wk "Find declaration")
    "i"  '(eglot-find-implementation :wk "Find implementation")
    "t"  '(eglot-find-typeDefinition :wk "Find type definition")
    "a"  '(eglot-code-actions :wk "Code actions")
    "r"  '(nil :wk "refactor")
    "rr" '(eglot-rename :wk "Rename")
    "rR" '(eglot-code-action-rewrite :wk "Rewrite")
    "rf" '(eglot-code-action-quickfix :wk "Quick fix")
    "ri" '(eglot-code-action-inline :wk "Inline")
    "re" '(eglot-code-action-extract :wk "Extract")
    "ro" '(eglot-code-action-organize-imports :wk "Organize imports")
    "eq" '(eglot-shutdown :wk "Shutdown")
    "er" '(eglot-reconnect :wk "Reconnect")
    "eQ" '(eglot-shutdown-all :wk "Shutdown all")
    "w"  '(eglot-show-workspace-configuration :wk "Eglot workspace config"))

  (+eglot-register
    '(c++-mode c++-ts-mode c-mode c-ts-mode)
    '("clangd"
      "--background-index"
      "-j=12"
      "--query-driver=/usr/bin/**/clang-*,/bin/clang,/bin/clang++,/usr/bin/gcc,/usr/bin/g++"
      "--clang-tidy"
      ;; "--clang-tidy-checks=*"
      "--all-scopes-completion"
      "--cross-file-rename"
      "--completion-style=detailed"
      "--header-insertion-decorators"
      "--header-insertion=iwyu"
      "--pch-storage=memory")
    "ccls")

  ;; From: https://github.com/MaskRay/ccls/wiki/eglot#misc
  (defun +eglot-ccls-inheritance-hierarchy (&optional derived)
    "Show inheritance hierarchy for the thing at point.
If DERIVED is non-nil (interactively, with prefix argument), show
the children of class at point."
    (interactive "P")
    (if-let* ((res (jsonrpc-request
                    (eglot--current-server-or-lose)
                    :$ccls/inheritance
                    (append (eglot--TextDocumentPositionParams)
                            `(:derived ,(if derived t :json-false))
                            '(:levels 100) '(:hierarchy t))))
              (tree (list (cons 0 res))))
        (with-help-window "*ccls inheritance*"
          (with-current-buffer standard-output
            (while tree
              (pcase-let ((`(,depth . ,node) (pop tree)))
                (cl-destructuring-bind (&key uri range) (plist-get node :location)
                  (insert (make-string depth ?\ ) (plist-get node :name) "\n")
                  (make-text-button
                   (+ (pos-bol 0) depth) (pos-eol 0)
                   'action (lambda (_arg)
                             (interactive)
                             (find-file (eglot--uri-to-path uri))
                             (goto-char (car (eglot--range-region range)))))
                  (cl-loop for child across (plist-get node :children)
                           do (push (cons (1+ depth) child) tree)))))))
      (eglot--error "Hierarchy unavailable"))))

(use-package consult-eglot
  :straight t
  :after consult eglot
  :config
  (+map :keymaps 'eglot-mode-map
    "cs" '(consult-eglot-symbols :wk "Symbols"))

  ;; Provide `consult-lsp' functionality from `consult-eglot', useful
  ;; for packages which relay on `consult-lsp' (like `dirvish-subtree').
  (unless (memq 'me-lsp minemacs-modules)
    (defalias 'consult-lsp-file-symbols #'consult-eglot-symbols)))

(use-package eldoc
  :straight (:type built-in)
  :defer t
  :custom
  (eldoc-documentation-strategy #'eldoc-documentation-compose))

(use-package eldoc-box
  :straight t
  :hook (prog-mode . eldoc-box-hover-at-point-mode)
  :hook (eglot-managed-mode . eldoc-box-hover-at-point-mode))

(use-package cov
  :straight (:type git :host github :repo "abougouffa/cov" :branch "feat/gcov-cmake")
  :defer t
  :custom
  (cov-highlight-lines t)
  :config
  (defun +cov-coverage-mode ()
    (interactive)
    (if cov-coverage-mode
        (progn
          (setq cov-coverage-mode nil)
          (message "Disabled coverage mode, showing how often lines are executed."))
      (setq cov-coverage-mode t)
      (message "Enabled coverage mode."))
    (cov-update)))

;;; Formatting
(use-package apheleia
  :straight t
  :general
  (+map "cff" #'apheleia-format-buffer)
  :config
  (add-to-list 'apheleia-formatters '(cmake-format . ("cmake-format")))
  (dolist (alist '((cmake-mode . cmake-format)
                   (cmake-ts-mode . cmake-format)
                   (cuda-mode . clang-format)
                   (common-lisp-mode . lisp-indent)
                   (emacs-lisp-mode . lisp-indent)
                   (lisp-data-mode . lisp-indent)))
    (add-to-list 'apheleia-mode-alist alist)))

(use-package editorconfig
  :straight t
  :general
  (+map
    "fc" '(editorconfig-find-current-editorconfig :wk "Find current EditorConfig")
    "cfe" #'editorconfig-format-buffer)
  :hook (prog-mode . editorconfig-mode))

(use-package clang-format
  :straight t
  :general
  (+map :keymaps '(c-mode-map c++-mode-map cuda-mode-map scad-mode-map)
    "cfc" #'clang-format-buffer))

;;; Modes
(use-package vimrc-mode
  :straight t
  :mode "\\.vim\\(rc\\)?\\'")

(use-package cmake-mode
  :mode "CMakeLists\\.txt\\'"
  :mode "\\.cmake\\'"
  :straight (:host github :repo "emacsmirror/cmake-mode" :files (:defaults "*")))

(use-package cmake-font-lock
  :straight (:host github :repo "Lindydancer/cmake-font-lock" :files (:defaults "*"))
  :hook (cmake-mode . cmake-font-lock-activate))

(use-package rust-mode
  :straight t
  :mode "\\.rs\\'"
  :config
  (+map-local :keymaps 'rust-mode-map
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
  :defer t
  :hook (cuda-mode . display-line-numbers-mode))

(use-package opencl-mode
  :straight t
  :mode "\\.cl\\'")

(use-package dumb-jump
  :straight t
  :commands
  +dumb-jump-hydra/body
  :custom
  (dumb-jump-selector 'completing-read)
  :init
  ;; Use as xref backend
  (with-eval-after-load 'xref
    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))
  :general
  (+map
    "cj" '(+dumb-jump-hydra/body :wk "+dumb-jump-hydra"))
  :config
  ;; Define Hydra keybinding (from the repo's examples)
  (defhydra +dumb-jump-hydra (:color blue :columns 3)
    "Dumb Jump."
    ("j" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Other window")
    ("e" dumb-jump-go-prefer-external "Go external")
    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
    ("i" dumb-jump-go-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
    ("b" dumb-jump-back "Back")))

(use-package hl-todo
  :straight (:host github :repo "tarsius/hl-todo")
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq
   hl-todo-keyword-faces
   (append
    hl-todo-keyword-faces
    '(("BUG" . "#ee5555")
      ("PROJ" . "#447f44")
      ("IDEA" . "#0fa050")))))

(use-package rainbow-mode
  :straight t
  :general
  (+map :keymaps '(prog-mode-map conf-mode-map text-mode-map)
    "tR" #'rainbow-mode))

(use-package lua-mode
  :straight t
  :defer t
  :custom
  (lua-indent-level 2))

(use-package powershell
  :straight t
  :defer t)

(use-package franca-idl
  :straight (:host github :repo "zeph1e/franca-idl.el")
  :defer t)

(use-package bnf-mode
  :straight t
  :defer t)

(use-package ebnf-mode
  :straight (:host github :repo "jeramey/ebnf-mode")
  :hook (bnf-mode . display-line-numbers-mode)
  :mode "\\.ebnf\\'")


(provide 'me-prog)

;;; me-prog.el ends here
