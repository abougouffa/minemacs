;;; me-debug.el --- Debugging stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(use-package dape
  :straight (:host github :repo "svaante/dape")
  :init
  (defvar +dape-cpptools-command
    (concat minemacs-local-dir "dape/cpptools/extension/bin/cpptools"))
  (defvar +dape-js-debug-path
    (concat minemacs-local-dir "dape/js-debug/extension/"))
  (defvar +dape-codelldb-command
    (concat minemacs-local-dir "dape/codelldb/extension/adapter/codelldb"))
  :custom
  (dape-inline-variables t)
  (dape-configs `((debugpy ; https://github.com/microsoft/debugpy
                   modes (python-ts-mode python-mode)
                   command "python3"
                   command-args ("-m" "debugpy.adapter")
                   :type "executable"
                   :request "launch"
                   :cwd dape-cwd-fn
                   :program dape-find-file-buffer-default)
                  (cpptools ; https://github.com/microsoft/vscode-cpptools
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
                  (codelldb ; https://github.com/vadimcn/codelldb
                   modes (c-mode c-ts-mode c++-mode c++-ts-mode rust-mode rust-ts-mode)
                   command ,+dape-codelldb-command
                   host "localhost"
                   port 5818
                   command-args ("--port" "5818")
                   :type "lldb"
                   :request "launch"
                   :cwd dape-cwd-fn
                   :program dape-find-file)
                  (delve ; https://github.com/go-delve/delve
                   modes (go-mode go-ts-mode)
                   command "dlv"
                   command-args ("dap" "--listen" "127.0.0.1:55878")
                   command-cwd dape-cwd-fn
                   host "127.0.0.1"
                   port 55878
                   :type "debug"
                   :request "launch"
                   :cwd dape-cwd-fn
                   :program dape-cwd-fn)
                  (js-debug ; https://github.com/microsoft/vscode-js-debug
                   modes (js-mode js-ts-mode)
                   host "localhost"
                   port 8123
                   command "node"
                   command-cwd ,+dape-js-debug-path
                   command-args ("src/dapDebugServer.js" "8123")
                   :type "pwa-node"
                   :request "launch"
                   :cwd dape-cwd-fn
                   :program dape-find-file-buffer-default
                   :outputCapture "console"
                   :sourceMapRenames t
                   :pauseForSourceMap nil
                   :enableContentValidation t
                   :autoAttachChildProcesses t
                   :console "internalConsole"
                   :killBehavior "forceful"))))

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
