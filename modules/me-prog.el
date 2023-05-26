;;; me-prog.el --- Programming stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(if (not (+emacs-features-p 'tree-sitter))
    ;; Use the external `tree-sitter' module
    (+load minemacs-modules-dir "obsolete/me-tree-sitter.el")

  ;; Use built-in `treesit' when available
  (use-package treesit
    :straight (:type built-in)
    :custom
    (treesit-font-lock-level 4))

  (use-package treesit-auto
    :straight (:host github :repo "renzmann/treesit-auto")
    :hook (minemacs-after-startup . global-treesit-auto-mode)
    :commands treesit-auto-install-all
    :custom
    (treesit-auto-install 'prompt)
    :init
    (+register-build-function! treesit-auto-install-all)
    :config
    ;; Install all languages when calling `treesit-auto-install-all'
    (setq treesit-language-source-alist (treesit-auto--build-treesit-source-alist))))

(use-package hideif
  :straight (:type built-in)
  :init
  ;; If `me-lsp' is enabled, `lsp-semantic-tokens-mode' should do a better job,
  ;; so we don't enable `hide-ifdef-mode'.
  (unless (memq 'me-lsp minemacs-modules)
    (dolist (h '(c++-mode-hook c++-ts-mode-hook c-mode-hook c-ts-mode-hook cuda-mode-hook))
      (add-hook h #'hide-ifdef-mode)))
  :custom
  (hide-ifdef-shadow t)
  (hide-ifdef-initially t))

(use-package eglot
  :straight `(:type ,(if (< emacs-major-version 29) 'git 'built-in))
  :commands +eglot-auto-enable
  :hook (eglot-managed-mode . eglot-inlay-hints-mode)
  :custom
  (eglot-autoshutdown t) ; shutdown after closing the last managed buffer
  (eglot-sync-connect 0) ; async, do not block
  (eglot-extend-to-xref t) ; can be interesting!
  (eglot-report-progress nil) ; disable annoying messages in echo area!
  :init
  ;; Register global keybinding
  (+map! :infix "c"
    "e"  '(nil :wk "eglot session")
    "ee" #'eglot
    "eA" #'+eglot-auto-enable)
  (defcustom +eglot-auto-enable-modes
    '(c++-mode c++-ts-mode c-mode c-ts-mode
      python-mode python-ts-mode
      rust-mode cmake-mode
      js-mode js-ts-mode typescript-mode typescript-ts-mode
      json-mode json-ts-mode js-json-mode)
    "Modes for which Eglot can be automatically enabled by `+eglot-auto-enable'."
    :group 'minemacs-prog
    :type '(repeat symbol))
  :config
  (defun +eglot-auto-enable ()
    "Auto-enable Eglot in configured modes in `+eglot-auto-enable-modes'."
    (interactive)
    (dolist (mode +eglot-auto-enable-modes)
      (let ((hook (intern (format "%s-hook" mode))))
        (add-hook hook #'eglot-ensure)
        (remove-hook hook #'lsp-deferred))))

  (+map! :keymaps 'eglot-mode-map
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

  ;; From: github.com/MaskRay/ccls/wiki/eglot#misc
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
  :init
  (+map! :keymaps 'eglot-mode-map
    "cs" '(consult-eglot-symbols :wk "Symbols"))
  :config
  ;; Provide `consult-lsp' functionality from `consult-eglot', useful for
  ;; packages that relays on `consult-lsp' (like `dirvish-subtree').
  (unless (memq 'me-lsp minemacs-modules)
    (defalias 'consult-lsp-file-symbols #'consult-eglot-symbols)))

(use-package eldoc
  :straight (:type built-in)
  :custom
  (eldoc-documentation-strategy #'eldoc-documentation-compose))

(use-package eldoc-box
  :straight t
  :hook (prog-mode . eldoc-box-hover-at-point-mode)
  :hook (eglot-managed-mode . eldoc-box-hover-at-point-mode))

(use-package cov
  :straight (:host github :repo "abougouffa/cov" :branch "feat/gcov-cmake")
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

(use-package compile
  :straight (:type built-in)
  :commands +toggle-burry-compilation-buffer-if-successful
  ;; Enable ANSI colors in compilation buffer
  :hook (compilation-filter . ansi-color-compilation-filter)
  :custom
  (compilation-scroll-output t) ; Keep scrolling the compilation buffer, `first-error' can be interesting
  (compilation-always-kill t) ; Always kill current compilation process before starting a new one
  (compilation-skip-visited t) ; Skip visited messages on compilation motion commands
  (compilation-window-height 12) ; Keep it readable
  :config
  ;; Integration of `compile' with `savehist'
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'compile-history))

  ;; Auto-close the compilation buffer if succeeded without warnings.
  ;; Adapted from: stackoverflow.com/q/11043004/3058915
  (defun +compilation--bury-if-successful-h (buf str)
    "Bury the compilation buffer if it succeeds without warnings."
    (when (and
           (string-match "compilation" (buffer-name buf))
           (string-match "finished" str)
           (not (with-current-buffer buf
                  (save-excursion
                    (goto-char (point-min))
                    (search-forward "warning" nil t)))))
      (run-with-timer
       3 nil
       (lambda (b)
         (with-selected-window (get-buffer-window b)
           (kill-buffer-and-window))
         (unless (current-message)
           (message "Compilation finished without warnings.")))
       buf)))

  (defun +toggle-burry-compilation-buffer-if-successful ()
    "Toggle auto-burying the successful compilation buffer."
    (interactive)
    (if (memq '+compilation--bury-if-successful-h compilation-finish-functions)
        (progn
          (message "Disabled burying compilation buffer.")
          (remove-hook 'compilation-finish-functions #'+compilation--bury-if-successful-h))
      (message "Enabled burying compilation buffer.")
      (add-hook 'compilation-finish-functions #'+compilation--bury-if-successful-h))))

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

(use-package cmake-mode
  :straight (:host github :repo "emacsmirror/cmake-mode" :files (:defaults "*"))
  :mode "CMakeLists\\.txt\\'"
  :mode "\\.cmake\\'")

(use-package cmake-font-lock
  :straight (:host github :repo "Lindydancer/cmake-font-lock" :files (:defaults "*"))
  :hook (cmake-mode . cmake-font-lock-activate))

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
  :commands
  +dumb-jump-hydra/body
  :custom
  (dumb-jump-selector 'completing-read)
  :init
  (+map!
    "cj" '(+dumb-jump-hydra/body :wk "+dumb-jump-hydra"))
  ;; Use as xref backend
  (with-eval-after-load 'xref
    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate 101))
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

(use-package hy-mode
  :straight t)

(use-package powershell
  :straight t)

(use-package franca-idl
  :straight (:host github :repo "zeph1e/franca-idl.el"))

(use-package bnf-mode
  :straight t)

(use-package ebnf-mode
  :straight (:host github :repo "jeramey/ebnf-mode")
  :hook (ebnf-mode . display-line-numbers-mode)
  :mode "\\.ebnf\\'")


(provide 'me-prog)

;;; me-prog.el ends here
