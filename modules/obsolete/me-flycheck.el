;;; me-flycheck.el --- Programming stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa and contributors

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(use-package flycheck
  :straight t
  :custom
  (flycheck-idle-change-delay 1.0)
  (flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (flycheck-buffer-switch-check-intermediate-buffers nil) ; maybe set it to t
  (flycheck-display-errors-delay 0.5)
  :init
  (defvar +flycheck-disabled-explicitly nil)
  (+load (file-name-directory (or load-file-name buffer-file-name)) "me-flycheck-cmake.el")
  (+load (file-name-directory (or load-file-name buffer-file-name)) "me-flycheck-eglot.el")
  :config
  (defun +flycheck-mode-toggle ()
    (interactive)
    (if (bound-and-true-p flycheck-mode)
        (progn
          (flycheck-mode -1)
          (setq +flycheck-disabled-explicitly t))
      (flycheck-mode 1)
      (setq +flycheck-disabled-explicitly nil)))

  (+map! "tc" #'+flycheck-mode-toggle)
  (+map!
    :keymaps 'flycheck-error-list-mode-map
    "j"   #'flycheck-error-list-next-error
    "k"   #'flycheck-error-list-previous-error
    "RET" #'flycheck-error-list-goto-error)

  ;; Use the current session load path when checking
  (setq flycheck-emacs-lisp-load-path 'inherit)

  ;; Integrate Flycheck with eldoc (not working with clangd!)
  (with-eval-after-load 'eldoc
    (add-hook
     'eldoc-mode-hook
     (defun +flycheck--register-documentation-function-h ()
       (add-hook 'eldoc-documentation-functions '+flycheck-eldoc-function 100 t)))

    (setopt eldoc-documentation-strategy #'eldoc-documentation-default ; combine docs
            flycheck-display-errors-function #'ignore) ; do not show

    (defun +flycheck-eldoc-function (report-doc &rest _)
      "Document diagnostics at point, intended for `eldoc-documentation-functions'."
      (when-let ((diags (flycheck-overlay-errors-at (point))))
        (propertize
         (funcall flycheck-help-echo-function diags)
         'help-echo-inhibit-substitution t)))))


(provide 'obsolete/me-flycheck)

;;; me-flycheck.el ends here
