;;; me-lsp.el --- Debugging and programming using lsp-mode and dap-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


(use-package lsp-mode
  :straight t
  :preface
  (setq lsp-use-plists t)
  :commands +lsp-auto-enable
  :custom
  (lsp-session-file (concat minemacs-local-dir "lsp/session.el"))
  (lsp-server-install-dir (concat minemacs-local-dir "lsp/servers/"))
  (lsp-keep-workspace-alive nil)
  (lsp-log-io nil)
  (lsp-idle-delay 1.0)
  (lsp-log-max (when minemacs-debug message-log-max))
  ;; Less intrusive UI
  (lsp-eldoc-render-all nil) ; clangd docs looks ugly on eldoc-box!
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-lens-enable nil)
  ;; maybe use nil and enable modes manually (lsp-completion-mode, lsp-modeline-diagnostics-mode, ...)
  (lsp-auto-configure t)
  ;; Those stuff should be managed by Emacs's builtins (whitespace-cleanup, treesit, ...)
  (lsp-semantic-tokens-enable nil) ; when t, hides unreachable ifdefs!
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-text-document-color nil)
  (lsp-trim-trailing-whitespace nil)
  (lsp-insert-final-newline nil)
  (lsp-trim-final-newlines nil)
  :init
  (+map!
    :infix "c"
    "l"  '(nil :wk "lsp session")
    "ll" #'lsp
    "lA" #'+lsp-auto-enable)
  (defvar +lsp-auto-enable-modes
    '(c++-mode c++-ts-mode c-mode c-ts-mode
      python-mode python-ts-mode
      rust-mode cmake-mode
      js-mode js-ts-mode typescript-mode typescript-ts-mode
      json-mode json-ts-mode js-json-mode))
  :config
  (defun +lsp-auto-enable ()
    (interactive)
    (dolist (mode +lsp-auto-enable-modes)
      (let ((hook (intern (format "%s-hook" mode))))
        (add-hook hook #'lsp-deferred)
        (remove-hook hook #'eglot-ensure))))

  (+map! :keymaps 'lsp-mode-map
    :infix "c"
    "fF" #'lsp-format-buffer
    "d"  '(lsp-find-declaration :wk "Find declaration")
    "D"  '(lsp-find-definition :wk "Find definition")
    "i"  '(lsp-find-implementation :wk "Find implementation")
    "t"  '(lsp-find-type-definition :wk "Find type definition")
    "a"  '(lsp-execute-code-action :wk "Code actions")
    "r"  '(nil :wk "refactor")
    "rr" '(lsp-rename :wk "Rename")
    "lq" '(lsp-workspace-shutdown :wk "Shutdown")
    "lr" '(lsp-workspace-restart :wk "Restart")))

(use-package ccls
  :straight t
  :custom
  ;; https://github.com/MaskRay/ccls/wiki/Customization#initialization-options
  ;; https://github.com/MaskRay/ccls/blob/master/src/config.hh
  (ccls-initialization-options
   '(:index (:comments 2
             :trackDependency 1
             :threads 4)
     :completion (:detailedLabel t
                  :caseSensitivity 1
                  :dropOldRequests t
                  :duplicateOptional t
                  :filterAndSort t
                  :maxNum 100
                  :placeholder t ; :json-false
                  :include (:maxPathSize 30
                            :blacklist []
                            :whitelist []
                            :suffixWhitelist [".h" ".hh" ".hxx" ".hpp" ".inc"]))
     :diagnostics (:onChange 1000 ; 1s
                   :onOpen 0
                   :onSave 0
                   :spellChecking t
                   :blacklist []
                   :whilist []
                   :caseSensitivity 1)
     :clang (:extraArgs ["--clang-tidy"]
             :excludeArgs []))))

(use-package lsp-pyright
  :straight t
  :after lsp-mode
  :demand t)

(use-package consult-lsp
  :straight t
  :after consult lsp-mode
  :demand t
  :config
  (+map! :keymaps 'lsp-mode-map
    "cs" '(consult-lsp-file-symbols :wk "Symbols")))

(use-package dap-mode
  :straight t
  :init
  (+map-local!
    :keymaps '(c-mode-map c++-mode-map python-mode-map
               rust-mode-map sh-mode-map bash-ts-mode-map
               js-mode-map js-ts-mode-map ruby-mode-map
               perl-mode-map)
    "d" '(nil :wk "dap")
    "dd" #'dap-debug
    "dt" #'dap-debug-edit-template
    "dh" #'dap-hydra/body)
  :hook (dap-stopped . (lambda (arg) (call-interactively #'dap-hydra)))
  :custom
  (dap-utils-extension-path (concat minemacs-local-dir "dap/extension/"))
  (dap-breakpoints-file (concat minemacs-local-dir "dap/breakpoints.el"))
  (dap-auto-configure-features '(locals breakpoints controls tooltip))
  (dap-auto-show-output nil))

(use-package dap-gdb-lldb
  :after dap-mode
  :demand t)

(use-package dap-cpptools
  :after dap-mode
  :demand t
  :custom
  (dap-cpptools-extension-version "1.13.8"))

(use-package dap-codelldb
  :after dap-mode
  :demand t
  :custom
  (dap-codelldb-extension-version "1.8.1"))

(use-package dap-python
  :after dap-mode
  :demand t)

(use-package dap-mouse
  :after dap-mode
  :demand t)


(provide 'me-lsp)
