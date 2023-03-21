;;; me-lsp.el --- Fast LSP client -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")


(use-package lsp-bridge
  :straight (:host github :repo "manateelazycat/lsp-bridge" :files (:defaults "*"))
  :commands +lsp-bridge-auto-enable
  :init
  (+map!
    :infix "c"
    "b"  '(nil :wk "lsp-bridge session")
    "bb" #'lsp-bridge-mode
    "bA" #'+lsp-bridge-auto-enable)
  (defcustom +lsp-bridge-auto-enable-modes
    '(c++-mode c++-ts-mode c-mode c-ts-mode
      python-mode python-ts-mode
      rust-mode rust-ts-mode cmake-mode
      js-mode js-ts-mode typescript-mode typescript-ts-mode
      json-mode json-ts-mode js-json-mode)
    "Modes for which LSP-mode can be automatically enabled by `+lsp-bridge-auto-enable'."
    :group 'minemacs
    :type '(repeat symbol))
  :config
  (defun +lsp-bridge-auto-enable ()
    "Auto-enable LSP-mode in configured modes in `+lsp-bridge-auto-enable-modes'."
    (interactive)
    (dolist (mode +lsp-bridge-auto-enable-modes)
      (let ((hook (intern (format "%s-hook" mode))))
        (add-hook hook #'lsp-bridge-mode)
        (remove-hook hook #'eglot-ensure)
        (remove-hook hook #'lsp-deferred))))

  (+map! :keymaps 'lsp-bridge-mode-map
    :infix "c"
    "fF" #'nil
    "d"  '(nil :wk "Find declaration")
    "D"  '(lsp-bridge-find-def :wk "Find definition")
    "i"  '(lsp-bridge-find-impl :wk "Find implementation")
    "i"  '(lsp-bridge-find-references :wk "Find references")
    "t"  '(nil :wk "Find type definition")
    "a"  '(lsp-bridge-code-action :wk "Code actions")
    "r"  '(nil :wk "refactor")
    "rr" '(lsp-bridge-rename :wk "Rename")
    "lq" '(nil :wk "Shutdown")
    "lr" '(nil :wk "Restart")))


(provide 'me-lsp-bridge)
