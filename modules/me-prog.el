;;; me-prog.el --- Programming stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>

(setq +treesitter-use-builtin (and (+emacs-features-p 'tree-sitter) nil))

;;; Tree sitter
(when +treesitter-use-builtin
  (use-package treesit-langs
    :straight (:host github :repo "kiennq/tree-sitter-langs" :files ("tree-sitter-langs-build.el" "treesit-*.el" "queries"))
    :when (+emacs-features-p 'tree-sitter)
    :after treesit
    :config
    (tree-sitter-langs-install-grammars t "0.12.63"))

  (with-eval-after-load 'treesit
    (defun +treesit-prefer-ts-modes (&rest modes)
      (let ((old-new-modes
             (mapcar
              (lambda (m)
                (let ((old (+symbol-or-car m))
                      (new (+symbol-or-cdr m)))
                  (cons (intern (format "%s-mode" old)) (intern (format "%s-ts-mode" new)))))
              modes))
            (num 0))
        (dolist (old-new-mode old-new-modes)
          (dolist (mode auto-mode-alist)
            (when (eq (cdr mode) (car old-new-mode))
              (setq auto-mode-alist (delete mode auto-mode-alist))
              (setq num (1+ num))
              (message "Replacing %s with %s." (symbol-name (car old-new-mode)) (symbol-name (cdr old-new-mode)))
              (add-to-list 'auto-mode-alist (cons (car mode) (cdr old-new-mode))))))
        num))

    ;; Prefer treesitter modes for supported languages
    (dolist (mode-lib '(c
                        json
                        python
                        typescript
                        (c++ . cpp)
                        (js . javascript)
                        (csharp . c-sharp)
                        ((sh . bash) . bash)))
      (let ((mode (+symbol-or-car mode-lib))
            (lib (+symbol-or-cdr mode-lib)))
        (when (treesit-language-available-p lib)
          (+treesit-prefer-ts-modes mode))))))

(unless +treesitter-use-builtin
  (use-package tree-sitter
    :straight t
    :defer t
    :hook ((python-mode
            c-mode
            c++-mode
            csharp-mode
            rust-mode
            json-mode
            xml-mode
            sh-mode
            typescript-ts-mode
            js-mode) . tree-sitter-mode))

  (use-package tree-sitter-hl
    :defer t
    :hook (tree-sitter-mode . tree-sitter-hl-mode))

  (use-package tree-sitter-langs
    :straight t
    :after tree-sitter)

  (use-package ts-fold
    :straight (:type git :host github :repo "emacs-tree-sitter/ts-fold")
    :defer t
    :hook (tree-sitter-mode . ts-fold-mode))

  ;; Needed by `ts-fold'
  (use-package fringe-helper
    :straight t
    :defer t)

  (use-package evil-textobj-tree-sitter
    :straight t
    :after evil tree-sitter
    :config
    ;; evil-textobj-tree-sitter comes with no default keybindings,
    ;; Here is the keybindings stolen from:
    ;; https://github.com/meain/dotfiles/blob/master/emacs/.config/emacs/init.el
    (define-key
     evil-outer-text-objects-map
     "m" (evil-textobj-tree-sitter-get-textobj "import"
           '((python-mode . [(import_statement) @import])
             (go-mode . [(import_spec) @import])
             (rust-mode . [(use_declaration) @import]))))
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
  :straight t
  :hook ((c++-mode
          c++-ts-mode
          c-mode c-ts-mode
          python-mode python-ts-mode
          rust-mode
          cmake-mode) . eglot-ensure)
  :init
  (+map
    "cF"  '(eglot-format-buffer :wk "Format buffer (eglot)")
    "cd"  '(eglot-find-declaration :wk "Find declaration")
    "ci"  '(eglot-find-implementation :wk "Find implementation")
    "ct"  '(eglot-find-typeDefinition :wk "Find type definition")
    "ca"  '(eglot-code-actions :wk "Code actions")
    "cr"  '(nil :wk "refactor")
    "crr" '(eglot-rename :wk "Rename")
    "crR" '(eglot-code-action-rewrite :wk "Rewrite")
    "crf" '(eglot-code-action-quickfix :wk "Quick fix")
    "cri" '(eglot-code-action-inline :wk "Inline")
    "cre" '(eglot-code-action-extract :wk "Extract")
    "cro" '(eglot-code-action-organize-imports :wk "Organize imports")
    "cs"  '(nil :wk "eglot session")
    "css" '(eglot :wk "Start")
    "csq" '(eglot-shutdown :wk "Shutdown")
    "csr" '(eglot-reconnect :wk "Reconnect")
    "csQ" '(eglot-shutdown-all :wk "Shutdown all")
    "cw"  '(eglot-show-workspace-configuration :wk "Eglot workspace config"))
  :custom
  (eglot-autoshutdown t) ;; shutdown after closing the last managed buffer
  (eglot-sync-connect 0) ;; async, do not block
  (eglot-extend-to-xref t) ;; can be interesting!
  :config

  ;; Prioritize ccls
  (add-to-list 'eglot-server-programs
               `((c++-mode c++-ts-mode c-mode c-ts-mode) . ,(eglot-alternatives
                                                             '("ccls" "clangd")))))


(use-package eldoc-box
  :straight t
  :after eldoc
  :config
  (eldoc-box-hover-at-point-mode 1))


;;; Debug
(use-package realgud
  :straight t
  :general
  (+map-local :keymaps '(c-mode-map c++-mode-map rust-mode-map python-mode-map)
    "d" `(,(+cmdfy!
            (pcase major-mode
             ('python-mode (realgud:pdb))
             ((or 'c-mode 'c++-mode) (realgud:gdb))))
          :wk "realgud"))
  :commands (realgud:gdb
             realgud:gud
             realgud:zshdb
             realgud:bashdb
             realgud:kshdb
             realgud:pdb
             realgud:pdb-remote))


(use-package realgud-lldb
  :straight t
  :general
  (+map-local :keymaps '(rust-mode-map)
    "d" `(#'realgud--lldb :wk "realgud"))
  :commands (realgud--lldb))


(use-package realgud-ipdb
  :straight t
  :commands (realgud:ipdb realgud:ipdb-remote))


;;; Formatting
(use-package apheleia
  :straight t
  :defer t
  :general
  (+map "cf" '(apheleia-format-buffer :wk "Format buffer"))
  :config
  (dolist (mode '(emacs-lisp-mode lisp-data-mode scheme-mode))
    (push (cons mode 'lisp-indent) apheleia-mode-alist)))


(use-package editorconfig
  :straight t
  :general
  (+map "fc" '(editorconfig-find-current-editorconfig :wk "Find current EditorConfig")))


(use-package clang-format
  :straight t
  :commands (clang-format
             clang-format-region
             clang-format-buffer))


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


;; (use-package flycheck
;;   :straight t
;;   :init
;;   (+map "tf" #'flycheck-mode)
;;   :config
;;   (setq flycheck-cppcheck-checks
;;         '("warning"
;;           "performance"
;;           "information"
;;           "style") ;; could be "all"
;;         flycheck-disabled-checkers '(python-flake8)))

;; (use-package me-flycheck-eglot
;;   :after eglot)

;; (use-package me-flycheck-cmake)

(use-package flymake
  :straight t
  :init
  (+map "tf" #'flymake-mode))

(use-package plantuml-mode
  :straight t
  :defer t
  :mode "\\.plantuml\\'"
  :custom
  (plantuml-jar-path (+expand 'local "plantuml/plantuml.jar"))
  :config
  (setq plantuml-default-exec-mode
        (cond ((executable-find "plantuml") 'executable)
              ((file-exists-p plantuml-jar-path) 'jar)
              (t (plantuml-download-jar) 'jar))))


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
  :defer t
  :init
  ;; use as xref backend
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))


(use-package hl-todo
  :straight (:host github :repo "tarsius/hl-todo")
  :hook (prog-mode . hl-todo-mode))


(use-package lua-mode
  :straight t
  :custom
  (lua-indent-level 2)
  :config
  (with-eval-after-load 'eglot
    (when (executable-find "lua-language-server")
      (add-to-list
       'eglot-server-programs
       `(lua-mode . ,(eglot-alternatives '(("lua-language-server") ("lua-lsp"))))))))


(use-package disaster
  :straight t
  :general
  (+map-local :keymaps '(c-mode-map c++-mode-map fortran-mode-map)
    "D" #'disaster))


(provide 'me-prog)

;;; me-prog.el ends here
