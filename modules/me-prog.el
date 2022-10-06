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
  :general
  (me-map
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
    "csQ" '(eglot-shutdown-all :which-key "Shutdown all")))


(use-package project-cmake
  :straight (:host github :repo "sawyerzheng/project-cmake" :branch "fix-unix-kits")
  :after eglot
  :general
  (me-map
    "pc"  '(nil :which-key "CMake")
    "pct" '(project-cmake-test :which-key "Test")
    "pcb" '(project-cmake-build :which-key "Build")
    "pcs" '(project-cmake-shell :which-key "Shell")
    "pci" '(project-cmake-install :which-key "Install")
    "pcc" '(project-cmake-configure :which-key "Configure"))
  :config
  (project-cmake-scan-kits)
  (project-cmake-eglot-integration))


;;; Debug
(use-package realgud
  :straight t
  :general
  (me-map-local :keymaps '(c-mode-map c++-mode-map rust-mode-map python-mode-map)
    "d" `(,(me-cmdfy!
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
  (me-map-local :keymaps '(rust-mode-map)
    "d" `((me-cmdfy! (pcase major-mode
                       ('python-mode (realgud:pdb))
                       ((or 'c-mode 'c++-mode) (realgud:gdb))))
          :which-key "realgud"))
  :commands (realgud--lldb))


(use-package realgud-ipdb
  :straight t
  :commands (realgud:ipdb realgud:ipdb-remote))


;;; Formatting
(use-package format-all
  :straight t
  :general
  (me-map "cf" '(format-all-buffer :which-key "Format buffer"))
  :commands (format-all-mode
             format-all-ensure-formatter
             format-all-buffer
             format-all-region))


(use-package editorconfig
  :straight t
  :general
  (me-map
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


(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))


(use-package highlight-numbers
  :straight t
  :hook (prog-mode . highlight-numbers-mode))


(use-package smartparens
  :straight t
  :hook (prog-mode . smartparens-mode)
  :hook (text-mode . smartparens-mode)
  :config

  (when nil
    (with-eval-after-load 'evil-collection
      ;; Make evil-mc cooperate with smartparens better
      (let ((vars (cdr (assq :default evil-mc-cursor-variables))))
        (unless (memq (car sp--mc/cursor-specific-vars) vars)
          (setcdr (assq :default evil-mc-cursor-variables)
                  (append vars sp--mc/cursor-specific-vars)))))))


(use-package flymake
  :straight t
  :general
  (me-map
    "tf" '(flymake-mode :which-key "Toggle flymake-mode")))


(use-package plantuml-mode
  :straight t
  :defer t
  :mode "\\.plantuml\\'"
  :config
  (setq plantuml-jar-path (expand-file-name "plantuml.jar" minemacs-var-dir)
        org-plantuml-jar-path plantuml-jar-path
        plantuml-default-exec-mode
        (cond ((executable-find "plantuml") 'executable)
              ((file-exists-p plantuml-jar-path) 'jar)
              (t (plantuml-download-jar) 'jar))))


(add-to-list 'auto-mode-alist '("\\.gitignore\\'"   . conf-mode))


(provide 'me-prog)

;;; me-prog.el ends here
