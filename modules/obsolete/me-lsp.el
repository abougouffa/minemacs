;;; me-lsp.el --- Debugging and programming using lsp-mode and dap-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2022-12-31
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:

(use-package lsp-mode
  :straight t
  :preface
  ;; HACK: Define the `$LSP_USE_PLISTS=true` to improve `lsp-mode' performances.
  ;; We set this environment variable here so we don't need to relay on the
  ;; system's environment variables.
  (setenv "LSP_USE_PLISTS" "true")
  (setq lsp-use-plists t)
  :custom
  (lsp-session-file (concat minemacs-local-dir "lsp/session.el"))
  (lsp-server-install-dir (concat minemacs-local-dir "lsp/servers/"))
  (lsp-keep-workspace-alive nil)
  (lsp-log-io nil)
  (lsp-idle-delay 1.0)
  (lsp-log-max (when minemacs-debug-p message-log-max))
  ;; Less intrusive UI
  (lsp-eldoc-render-all nil) ; clangd docs looks ugly on eldoc-box!
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-lens-enable nil)
  ;; Maybe set to nil and enable modes manually (`lsp-completion-mode',
  ;; `lsp-modeline-diagnostics-mode', ...)
  (lsp-auto-configure t)
  ;; Those stuff should be managed by Emacs's builtins (whitespace-cleanup, treesit, ...)
  (lsp-semantic-tokens-enable t) ; when t, hides unreachable ifdefs!
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-text-document-color nil)
  (lsp-trim-trailing-whitespace nil)
  (lsp-insert-final-newline nil)
  (lsp-trim-final-newlines nil)
  :init
  (defcustom +lsp-auto-enable-modes
    '(c-mode c++-mode c-ts-base-mode python-base-mode rust-mode
      rust-ts-mode cmake-mode js-base-mode typescript-mode
      typescript-ts-base-mode json-mode json-ts-mode js-json-mode)
    "Modes for which LSP-mode can be automatically enabled by `+lsp-auto-enable'."
    :group 'minemacs-prog
    :type '(repeat symbol))
  (defun +lsp--ensure-maybe-h ()
    "Maybe auto start LSP if the current mode is in `+lsp-auto-enable-modes'."
    (when (memq major-mode +lsp-auto-enable-modes)
      (lsp-deferred)))
  (defun +lsp-auto-enable ()
    "Auto-enable LSP-mode in configured modes in `+lsp-auto-enable-modes'."
    (interactive)
    (add-hook 'after-change-major-mode-hook #'+lsp--ensure-maybe-h)
    (remove-hook 'after-change-major-mode-hook #'+eglot--ensure-maybe-h)))

(use-package ccls
  :straight t
  :custom
  ;; Customization of CCLS, see:
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
  :demand)

(use-package consult-lsp
  :straight t
  :after consult lsp-mode)

(use-package dap-mode
  :straight t
  :hook (dap-stopped . (lambda (arg) (call-interactively #'dap-hydra)))
  :custom
  (dap-utils-extension-path (concat minemacs-local-dir "dap/extension/"))
  (dap-breakpoints-file (concat minemacs-local-dir "dap/breakpoints.el"))
  (dap-auto-configure-features '(locals breakpoints controls tooltip))
  (dap-auto-show-output nil))

(use-package dap-gdb-lldb
  :after dap-mode
  :demand
  :custom
  (dap-gdb-lldb-extension-version
   (+github-latest-release "WebFreak001/code-debug" "0.27.0" t)))

(use-package dap-cpptools
  :after dap-mode
  :demand
  :custom
  (dap-cpptools-extension-version
   (+github-latest-release "microsoft/vscode-cpptools" "1.18.5" t)))

(use-package dap-codelldb
  :after dap-mode
  :demand
  :custom
  (dap-codelldb-extension-version
   (+github-latest-release "vadimcn/codelldb" "1.10.0" t)))

(use-package dap-python
  :after dap-mode
  :demand
  :custom
  (dap-python-debugger 'debugpy)) ; the default `ptvsd' has been deprecated

(use-package dap-mouse
  :after dap-mode
  :demand)


(provide 'obsolete/me-lsp)
;;; me-lsp.el ends here
