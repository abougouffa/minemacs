;; me-use-package-extra.el --- Extend use-package to allow straight-x package pinning -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;; ## Use-package & straight.el extensions

;; - `:trigger-commands' - allow loading the package before executing a specific
;;   external command/function using the `:trigger-commands' keyword.

;;; Code:

(require 'me-lib)
(require 'use-package)


;; When only built-in packages are loaded, we define `:straight' to do nothing.
;; This is important for the updates installed for built-in packages in
;; `me-builtin' like `transient', `eglot', `xref', etc.
(when minemacs-builtin-only-p
  (add-to-list 'use-package-keywords :straight)
  (defalias 'use-package-normalize/:straight #'ignore)
  (defun use-package-handler/:straight (name _keyword _arg rest state)
    (use-package-concat (use-package-process-keywords name rest state))))

;;; Extend `use-package'

(add-to-list 'use-package-keywords :trigger-commands)

;; `:trigger-commands' implementation
(defun use-package-normalize/:trigger-commands (name keyword args)
  (setq args (use-package-normalize-recursive-symlist name keyword args))
  (if (consp args) args (list args)))

(defun use-package-handler/:trigger-commands (name _keyword arg rest state)
  (use-package-concat
   (unless (plist-get state :demand)
     `((satch-add-advice ',(delete-dups arg) :before (lambda (&rest _args) (require ',name)) nil :transient t)))
   (use-package-process-keywords name rest state)))


(provide 'me-use-package-extra)

;;; me-use-package-extra.el ends here
