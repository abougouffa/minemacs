;;; me-checkers.el --- Syntax checking -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


(use-package flymake
  :straight (:type built-in)
  :general
  (+map "tf" #'flymake-mode)
  :config
  (+map-local :keymaps 'flymake-mode-map
    "f"  '(nil :wk "flymake")
    "fn" #'flymake-goto-next-error
    "fN" #'flymake-goto-prev-error
    "fs" #'flymake-start))

(use-package flycheck
  :straight t
  :custom
  (flycheck-idle-change-delay 1.0)
  (flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (flycheck-buffer-switch-check-intermediate-buffers t)
  (flycheck-display-errors-delay 0.2)
  :general
  (+map "tc" #'flycheck-mode)
  :config
  (+map
    :keymaps 'flycheck-error-list-mode-map
    "j"   #'flycheck-error-list-next-error
    "k"   #'flycheck-error-list-previous-error
    "RET" #'flycheck-error-list-goto-error)

  ;; Use the current session load path when checking
  (setq flycheck-emacs-lisp-load-path 'inherit)

  ;; Integrate Flycheck with eldoc (not working with clangd!)
  (with-eval-after-load 'eldoc
    (add-hook 'eldoc-documentation-functions '+flycheck-eldoc-function t t)
    (setq eldoc-documentation-strategy #'eldoc-documentation-compose)

    (setq flycheck-display-errors-function nil) ; do not show

    (defun +flycheck-eldoc-function (report-doc &rest _)
      "Document diagnostics at point, intended for `eldoc-documentation-functions'."
      (when-let ((diags (flycheck-overlay-errors-at (point))))
        (propertize
         (funcall flycheck-help-echo-function diags)
         'help-echo-inhibit-substitution t)))))

(use-package me-flycheck-eglot
  :after flycheck eglot)

(use-package me-flycheck-cmake
  :after flycheck)

(provide 'me-checkers)
