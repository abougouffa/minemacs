;;; me-prog.el --- Programming stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>

(use-package treesit-langs
  :straight (:host github :repo "kiennq/treesit-langs" :files (:defaults "queries"))
  :when (>= emacs-major-version 29)
  :hook ((c-ts-mode
          c++-ts-mode
          rust-ts-mode
          go-ts-mode
          go-mod-ts-mode
          csharp-ts-mode
          bash-ts-mode
          cmake-ts-mode
          dockerfile-ts-mode
          python-ts-mode
          js-ts-mode
          tsx-ts-mode
          typescript-ts-mode
          java-ts-mode
          css-ts-mode
          json-ts-mode
          toml-ts-mode
          yaml-ts-mode)
         . treesit-hl-enable))

(use-package treesit
  :straight (:type built-in)
  :defer t
  :config
  (setq-default treesit-font-lock-level 4))

(with-eval-after-load 'minemacs-loaded
  (+fn-inhibit-messages! tree-sitter-langs-install-grammars)
  (+fn-inhibit-messages! tsc-dyn-get-ensure)

  (use-package tree-sitter
    :straight t
    :defer 5
    :hook (tree-sitter-after-on . tree-sitter-hl-mode)
    :config
    (global-tree-sitter-mode 1))

  (use-package tree-sitter-langs
    :straight t
    :after tree-sitter)

  (use-package evil-textobj-tree-sitter
    :straight t
    :after evil tree-sitter
    :config
    ;; evil-textobj-tree-sitter comes with no default keybindings,
    ;; Here is a keybindings (vaX) stolen from here:
    ;; https://github.com/meain/dotfiles/blob/master/emacs/.config/emacs/init.el
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
    (define-key
     evil-outer-text-objects-map
     "f" (cons "evil-outer-function" (evil-textobj-tree-sitter-get-textobj "function.outer")))
    (define-key
     evil-inner-text-objects-map
     "f" (cons "evil-inner-function" (evil-textobj-tree-sitter-get-textobj "function.inner")))
    (define-key
     evil-outer-text-objects-map
     "c" (cons "evil-outer-class" (evil-textobj-tree-sitter-get-textobj "class.outer")))
    (define-key
     evil-inner-text-objects-map
     "c" (cons "evil-inner-class" (evil-textobj-tree-sitter-get-textobj "class.inner")))
    (define-key
     evil-outer-text-objects-map
     "n" (cons "evil-outer-comment" (evil-textobj-tree-sitter-get-textobj "comment.outer")))
    (define-key
     evil-inner-text-objects-map
     "n" (cons "evil-outer-comment" (evil-textobj-tree-sitter-get-textobj "comment.outer")))
    (define-key
     evil-outer-text-objects-map
     "o" (cons "evil-outer-loop" (evil-textobj-tree-sitter-get-textobj "loop.outer")))
    (define-key
     evil-inner-text-objects-map
     "o" (cons "evil-inner-loop" (evil-textobj-tree-sitter-get-textobj "loop.inner")))
    (define-key
     evil-outer-text-objects-map
     "v" (cons "evil-outer-conditional" (evil-textobj-tree-sitter-get-textobj "conditional.outer")))
    (define-key
     evil-inner-text-objects-map
     "v" (cons "evil-inner-conditional" (evil-textobj-tree-sitter-get-textobj "conditional.inner")))
    (define-key
     evil-inner-text-objects-map
     "a" (cons "evil-inner-parameter" (evil-textobj-tree-sitter-get-textobj "parameter.inner")))
    (define-key
     evil-outer-text-objects-map
     "a" (cons "evil-outer-parameter" (evil-textobj-tree-sitter-get-textobj "parameter.outer")))

    (define-key
     evil-normal-state-map
     (kbd "]a") (cons "goto-parameter-start" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "parameter.inner"))))
    (define-key
     evil-normal-state-map
     (kbd "[a") (cons "goto-parameter-start" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "parameter.inner" t))))
    (define-key
     evil-normal-state-map
     (kbd "]A") (cons "goto-parameter-end" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "parameter.inner" nil t))))
    (define-key
     evil-normal-state-map
     (kbd "[A") (cons "goto-parameter-end" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "parameter.inner" t t))))
    (define-key
     evil-normal-state-map
     (kbd "]v") (cons "goto-conditional-start" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "conditional.outer"))))
    (define-key
     evil-normal-state-map
     (kbd "[v") (cons "goto-conditional-start" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "conditional.outer" t))))
    (define-key
     evil-normal-state-map
     (kbd "]V") (cons "goto-conditional-end" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "conditional.outer" nil t))))
    (define-key
     evil-normal-state-map
     (kbd "[V") (cons "goto-conditional-end" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "conditional.outer" t t))))
    (define-key
     evil-normal-state-map
     (kbd "]c") (cons "goto-class-start" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "class.outer"))))
    (define-key
     evil-normal-state-map
     (kbd "[c") (cons "goto-class-start" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "class.outer" t))))
    (define-key
     evil-normal-state-map
     (kbd "]C") (cons "goto-class-end" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "class.outer" nil t))))
    (define-key
     evil-normal-state-map
     (kbd "[C") (cons "goto-class-end" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "class.outer" t t))))
    (define-key
     evil-normal-state-map
     (kbd "]n") (cons "goto-comment-start" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "comment.outer"))))
    (define-key
     evil-normal-state-map
     (kbd "[n") (cons "goto-comment-start" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "comment.outer" t))))
    (define-key
     evil-normal-state-map
     (kbd "]N") (cons "goto-comment-end" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "comment.outer" nil t))))
    (define-key
     evil-normal-state-map
     (kbd "[N") (cons "goto-comment-end" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "comment.outer" t t))))
    (define-key
     evil-normal-state-map
     (kbd "]f") (cons "goto-function-start" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "function.outer") (reposition-window))))
    (define-key
     evil-normal-state-map
     (kbd "[f") (cons "goto-function-start" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "function.outer" t) (reposition-window))))
    (define-key
     evil-normal-state-map
     (kbd "]F") (cons "goto-function-end" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "function.outer" nil t) (reposition-window))))
    (define-key
     evil-normal-state-map
     (kbd "[F") (cons "goto-function-end" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "function.outer" t t) (reposition-window))))))

;;; Eglot + LSP
(use-package eglot
  :straight `(:type ,(if (< emacs-major-version 29) 'git 'built-in))
  :hook ((c++-mode
          c++-ts-mode
          c-mode c-ts-mode
          python-mode python-ts-mode
          rust-mode
          cmake-mode) . eglot-ensure)
  :commands eglot eglot-ensure
  :general
  (+map
    :infix "c"
    "S"  '(nil :wk "eglot session")
    "Ss" '(eglot :wk "Start"))
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
    "Sq" '(eglot-shutdown :wk "Shutdown")
    "Sr" '(eglot-reconnect :wk "Reconnect")
    "SQ" '(eglot-shutdown-all :wk "Shutdown all")
    "w"  '(eglot-show-workspace-configuration :wk "Eglot workspace config"))
  :custom
  (eglot-autoshutdown t) ;; shutdown after closing the last managed buffer
  (eglot-sync-connect 0) ;; async, do not block
  (eglot-extend-to-xref t) ;; can be interesting!
  :config
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
  :general
  (+map :keymaps 'eglot-mode-map
    "cs" '(consult-eglot-symbols :wk "Symbols")))

(use-package eldoc-box
  :straight t
  :hook (prog-mode . eldoc-box-hover-at-point-mode))

;;; Formatting
(use-package apheleia
  :straight t
  :general
  (+map "cff" #'apheleia-format-buffer)
  :config
  (dolist (mode '(emacs-lisp-mode lisp-data-mode scheme-mode))
    (push (cons mode 'lisp-indent) apheleia-mode-alist)))

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

(use-package flymake
  :straight (:type built-in)
  :general
  (+map
    "tf"  #'flymake-mode)
  (+map-local :keymaps 'flymake-mode-map
    "f"  '(nil :wk "flymake")
    "fn" #'flymake-goto-next-error
    "fN" #'flymake-goto-prev-error
    "fs" #'flymake-start))

(use-package flymake-easy
  :straight t
  :defer t)

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

(use-package eldoc-cmake
  :straight t
  :after cmake-mode)

(use-package plantuml-mode
  :straight t
  :mode "\\.plantuml\\'"
  :hook (plantuml-mode . +plantuml-mode-setup)
  :custom
  (plantuml-jar-path (concat minemacs-local-dir "plantuml/plantuml.jar"))
  (plantuml-indent-level 2)
  :config
  (setq
   plantuml-default-exec-mode
   ;; Prefer the system executable
   (if (executable-find "plantuml")
       'executable
     ;; Then, if a JAR exists, use it
     (or (and (file-exists-p plantuml-jar-path) 'jar)
         ;; otherwise, try to download a JAR in interactive mode
         (and (not noninteractive) (plantuml-download-jar) 'jar)
         ;; Fall back to server
         'server)))

  ;; Add support fot capf, rather than the builtin `plantuml-complete-symbol'
  (defun +plantuml-mode-setup ()
    (add-to-list
     'completion-at-point-functions
     (defun +plantuml-complete-at-point ()
       "Perform symbol-at-pt completion on word before cursor."
       (let* ((end-pos (point))
              (sym-at-pt (or (thing-at-point 'symbol) ""))
              (max-match (try-completion sym-at-pt plantuml-kwdList)))
         (unless (null max-match)
           (list (- end-pos (length sym-at-pt))
                 end-pos
                 (if (eq max-match t)
                     (list keyword)
                   (all-completions sym-at-pt plantuml-kwdList))))))))

  (+map-local :keymaps 'plantuml-mode-map
    "p" #'plantuml-preview-buffer
    "P" #'plantuml-preview
    "d" `(,(+cmdfy!
            (if plantuml-mode-debug-enabled
                (plantuml-disable-debug)
              (plantuml-enable-debug)))
          :wk "Toggle debug")))

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


(provide 'me-prog)

;;; me-prog.el ends here
