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
  :hook ((c-mode c++-mode rust-mode python-mode latex-mode LaTeX-mode) . eglot-ensure)
  :init
  (+map
    "ca"  '(eglot-code-actions :which-key "Code actions")
    "cq"  '(eglot-code-action-quickfix :which-key "Code action quickfix")
    "cF"  '(eglot-format-buffer :which-key "Format buffer (eglot)")
    "cr"  '(nil :which-key "refactor")
    "crr" '(eglot-rename :which-key "Rename")
    "cd"  '(eglot-find-declaration :which-key "Find declaration")
    "ci"  '(eglot-find-implementation :which-key "Find implementation")
    "ct"  '(eglot-find-typeDefinition :which-key "Find type definition")
    "cs"  '(nil :which-key "eglot session")
    "css" '(eglot :which-key "Start")
    "csq" '(eglot-shutdown :which-key "Shutdown")
    "csr" '(eglot-reconnect :which-key "Reconnect")
    "csQ" '(eglot-shutdown-all :which-key "Shutdown all"))
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
(use-package format-all
  :straight t
  :general
  (+map "cf" '(format-all-buffer :which-key "Format buffer"))
  :commands (format-all-mode
             format-all-ensure-formatter
             format-all-buffer
             format-all-region))


(use-package editorconfig
  :straight t
  :general
  (+map
    "fc" '(editorconfig-find-current-editorconfig :which-key "Open current editorconfig")))


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
  (+map :keymaps 'rust-mode-map
    "C" '(nil :which-key "compile/test")
    "Cc" #'rust-compile
    "CC" #'rust-compile-release
    "Ck" #'rust-check
    "Ct" #'rust-test
    "Cr" #'rust-run
    "CR" #'rust-run
    "Cy" #'rust-run-clippy
    "Cf" #'rust-format-buffer
    "CF" #'rust-goto-format-problem))


(use-package dumb-jump
  :straight t
  :defer t
  :init
  ;; use as xref backend
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))


(provide 'me-prog)

;;; me-prog.el ends here
