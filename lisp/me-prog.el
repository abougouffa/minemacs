;;; prog.el --- Programming stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>

;;; Tree sitter
(use-package tree-sitter
  :straight t
  :after minemacs-loaded
  :config
  (global-tree-sitter-mode))

(use-package tree-sitter-hl
  :hook (tree-sitter-mode . tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :straight t
  :after tree-sitter)

;;; Eglot + LSP
(use-package eglot
  :straight t
  :commands eglot
  :config
  ;; A hack to make Eglot work with Projectile
  (when (featurep 'projectile)
    (defun me-projectile-project-find-function (dir)
      (let ((root (projectile-project-root dir)))
        (and root (cons 'transient root)))

      (with-eval-after-load 'project
        (add-to-list 'project-find-functions 'projectile-project-find-function)))))

(use-package project-cmake
  :straight (:type git :host github :repo "juanjosegarciaripoll/project-cmake")
  :defer t
  :config
  (require 'eglot)
  (project-cmake-scan-kits)
  (project-cmake-eglot-integration))

;;; Debug
(use-package realgud
  :straight t
  :commands (realgud:gdb
             realgud:gud
             realgud:zshdb
             realgud:bashdb
             realgud:kshdb
             realgud:pdb
             realgud:pdb-remote))

(use-package realgud-lldb
  :straight t
  :commands (realgud--lldb))


(use-package realgud-ipdb
  :straight t
  :commands (realgud:ipdb realgud:ipdb-remote))


;;; Formatting
(use-package format-all
  :straight t
  :general
  (me-global-def "cf" '(format-all-buffer :which-key "Format buffer"))
  :commands (format-all-mode
             format-all-ensure-formatter
             format-all-buffer
             format-all-region))


(use-package editorconfig
  :straight t
  :general
  (me-global-def
    "fc" '(editorconfig-find-current-editorconfig :which-key "Open current editorconfig")))


(use-package clang-format
  :straight t
  :commands (clang-format
             clang-format-region
             clang-format-buffer))


(use-package vimrc-mode
  :straight t
  :mode "\\.vim\\(rc\\)?\\'")


(use-package cmake-mode
  :mode "CMakeLists\\.txt\\'"
  :mode "\\.cmake\\'"
  :straight (:type git :host github :repo "emacsmirror/cmake-mode" :files (:defaults "*")))


(use-package cmake-font-lock
  :straight (:type git :host github :repo "Lindydancer/cmake-font-lock" :files (:defaults "*"))
  :defer t)


(provide 'me-prog)

;;; prog.el ends here
