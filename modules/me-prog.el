;;; me-prog.el --- Programming stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>

;;; Tree sitter
(use-package tree-sitter
  :straight t
  :hook (python-mode . tree-sitter-mode)
  :hook (c-mode . tree-sitter-mode)
  :hook (c++-mode . tree-sitter-mode)
  :hook (rust-mode . tree-sitter-mode)
  :hook (json-mode . tree-sitter-mode)
  :hook (xml-mode . tree-sitter-mode))


(use-package tree-sitter-hl
  :hook (tree-sitter-mode . tree-sitter-hl-mode))


(use-package tree-sitter-langs
  :straight t
  :after tree-sitter)


;;; Eglot + LSP
(use-package eglot
  :straight t
  :defer t
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
  (eglot-extend-to-xref t)) ;; can be interesting!


(use-package project-cmake
  :straight (:host github :repo "juanjosegarciaripoll/project-cmake")
  :commands (project-cmake-test
             project-cmake-build
             project-cmake-shell
             project-cmake-install
             project-cmake-configure)
  :init
  (+map :keymaps '(c++-mode-map c-mode-map)
    "pC"  '(nil :wk "cmake")
    "pCt" '(project-cmake-test :wk "Test")
    "pCb" '(project-cmake-build :wk "Build")
    "pCs" '(project-cmake-shell :wk "Shell")
    "pCi" '(project-cmake-install :wk "Install")
    "pCc" '(project-cmake-configure :wk "Configure"))
  :config
  (project-cmake-scan-kits)
  (project-cmake-eglot-integration))


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
  (+map
    "fc" '(editorconfig-find-current-editorconfig :wk "Find current EditorConfig")))


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


(use-package flymake
  :straight t
  :general
  (+map
    "tf" #'flymake-mode))


(use-package plantuml-mode
  :straight t
  :defer t
  :mode "\\.plantuml\\'"
  :custom
  (plantuml-jar-path (expand-file-name "plantuml.jar" minemacs-local-dir))
  (org-plantuml-jar-path plantuml-jar-path)
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
