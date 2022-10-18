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
    "cF"  '(eglot-format-buffer :which-key "Format buffer (eglot)")
    "cd"  '(eglot-find-declaration :which-key "Find declaration")
    "ci"  '(eglot-find-implementation :which-key "Find implementation")
    "ct"  '(eglot-find-typeDefinition :which-key "Find type definition")
    "ca"  '(eglot-code-actions :which-key "Code actions")
    "cr"  '(nil :which-key "refactor")
    "crr" '(eglot-rename :which-key "Rename")
    "crR" '(eglot-code-action-rewrite :which-key "Rewrite")
    "crf" '(eglot-code-action-quickfix :which-key "Quick fix")
    "cri" '(eglot-code-action-inline :which-key "Inline")
    "cre" '(eglot-code-action-extract :which-key "Extract")
    "cro" '(eglot-code-action-organize-imports :which-key "Organize imports")
    "cs"  '(nil :which-key "eglot session")
    "css" '(eglot :which-key "Start")
    "csq" '(eglot-shutdown :which-key "Shutdown")
    "csr" '(eglot-reconnect :which-key "Reconnect")
    "csQ" '(eglot-shutdown-all :which-key "Shutdown all")
    "cw"  '(eglot-show-workspace-configuration :which-key "Eglot workspace config"))
  :custom
  (eglot-autoshutdown t) ;; shutdown after closing the last managed buffer
  (eglot-sync-connect 0) ;; async, do not block
  (eglot-extend-to-xref t)) ;; can be interesting!


(use-package project-cmake
  :straight (:host github :repo "sawyerzheng/project-cmake" :branch "fix-unix-kits")
  :commands (project-cmake-test
             project-cmake-build
             project-cmake-shell
             project-cmake-install
             project-cmake-configure)
  :init
  (+map :keymaps '(c++-mode-map c-mode-map)
    "pC"  '(nil :which-key "cmake")
    "pCt" '(project-cmake-test :which-key "Test")
    "pCb" '(project-cmake-build :which-key "Build")
    "pCs" '(project-cmake-shell :which-key "Shell")
    "pCi" '(project-cmake-install :which-key "Install")
    "pCc" '(project-cmake-configure :which-key "Configure"))
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
          :which-key "realgud"))
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
    "d" `(#'realgud--lldb :which-key "realgud"))
  :commands (realgud--lldb))


(use-package realgud-ipdb
  :straight t
  :commands (realgud:ipdb realgud:ipdb-remote))


;;; Formatting
(use-package apheleia
  :straight t
  :defer t
  :general
  (+map "cf" '(apheleia-format-buffer :which-key "Format buffer"))
  :config
  (dolist (mode '(emacs-lisp-mode lisp-data-mode scheme-mode))
    (push (cons mode 'lisp-indent) apheleia-mode-alist)))


(use-package editorconfig
  :straight t
  :general
  (+map
    "fc" '(editorconfig-find-current-editorconfig :which-key "Find current EditorConfig")))


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


(provide 'me-prog)

;;; me-prog.el ends here
