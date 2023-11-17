;;; me-debug.el --- Debugging stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(use-package dape
  :straight (:host github :repo "svaante/dape")
  :init
  (defvar +dape-cpptools-command
    (concat minemacs-local-dir "dape/microsoft.vscode-cpptools/extension/bin/cpptools"))
  (defvar +dape-codelldb-command
    (concat minemacs-local-dir "dape/vadimcn.codelldb/extension/adapter/codelldb"))
  :custom
  (dape-inline-variables t)
  (dape-configs `((debugpy
                   modes (python-ts-mode python-mode)
                   command "python3"
                   command-args ("-m" "debugpy.adapter")
                   :type "executable"
                   :request "launch"
                   :cwd dape-cwd-fn
                   :program dape-find-file-buffer-default)
                  (godot-launch
                   modes (gdscript-mode)
                   host "127.0.0.1"
                   port 6006
                   :type "server"
                   :request "launch")
                  (cpptools
                   modes (c-mode c-ts-mode c++-mode c++-ts-mode)
                   command-cwd ,(file-name-directory +dape-cpptools-command)
                   command +dape-cpptools-command
                   :type "cpptools"
                   :request "launch"
                   :cwd dape-cwd-fn
                   :program dape-find-file
                   :MIMode ,(cond
                             ((executable-find "gdb") "gdb")
                             ((executable-find "lldb") "lldb")))
                  (codelldb
                   modes (c-mode c-ts-mode c++-mode c++-ts-mode rust-mode rust-ts-mode)
                   command ,+dape-codelldb-command
                   host "localhost"
                   port 5818
                   command-args ("--port" "5818")
                   :type "lldb"
                   :request "launch"
                   :cwd dape-cwd-fn
                   :program dape-find-file))))

(use-package realgud
  :straight (realgud :build (:not compile))
  :init
  (+map-local! :keymaps '(c-mode-map c++-mode-map python-mode-map
                          c-ts-mode-map c++-ts-mode-map python-ts-mode-map
                          rust-mode-map rust-ts-mode-map
                          sh-mode-map bash-ts-mode-map)
    "r" '(nil :wk "realgud")
    "rd" #'+realgud:start
    "rh" #'+realgud-hydra/body))

(use-package realgud-lldb
  :straight t
  :init
  (defalias 'realgud:lldb #'realgud--lldb)
  :commands (realgud--lldb realgud:lldb lldb))

(use-package realgud-ipdb
  :straight t
  :commands (ipdb realgud:ipdb))

(use-package disaster
  :straight t
  :when (executable-find "objdump")
  :init
  (+map-local! :keymaps '(c-mode-map c++-mode-map fortran-mode-map)
    "D" #'disaster))


(provide 'me-debug)

;;; me-debug.el ends here
