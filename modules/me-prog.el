;;; me-prog.el --- Programming stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>

(setq +treesitter-use-builtin (and feat/treesitter nil))

;;; Tree sitter
(when +treesitter-use-builtin
  (use-package treesit-langs
    :straight (:host github :repo "kiennq/tree-sitter-langs" :files ("tree-sitter-langs-build.el" "treesit-*.el" "queries"))
    :when feat/treesitter
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


(use-package me-external-tree-sitter
  :unless +treesitter-use-builtin)


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


(use-package flycheck
  :straight t
  :init
  (+map "tf" #'flycheck-mode)
  :config
  (setq flycheck-cppcheck-checks '("all")))


(use-package plantuml-mode
  :straight t
  :defer t
  :mode "\\.plantuml\\'"
  :custom
  (plantuml-jar-path (concat minemacs-local-dir "plantuml.jar"))
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


(provide 'me-prog)

;;; me-prog.el ends here
