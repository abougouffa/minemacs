;; me-use-package-extra.el --- Extend use-package to allow straight-x package pinning -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;; ## Use-package & straight.el extensions

;; - :pin-ref - allow pinning versions to a specific revision from `use-package'
;;   using the `:pin-ref' keyword. See:
;;   github.com/radian-software/straight.el#how-do-i-pin-package-versions-or-use-only-tagged-releases
;;
;; - `:trigger-commands' - allow loading a package before executing a specific
;;   command/function using the `:trigger-commands' keyword
;;
;; - Better handling of conditionally enabled packages.

;;; Code:

(eval-when-compile
  (require 'straight)
  (require 'use-package))

(with-eval-after-load 'straight
  ;; Add a profile (and lockfile) for stable package revisions.
  (add-to-list 'straight-profiles '(pinned . "pinned.el"))
  (require 'straight-x))

(with-eval-after-load 'use-package-core
  (add-to-list 'use-package-keywords :pin-ref)
  (add-to-list 'use-package-keywords :trigger-commands)

  ;; :trigger-commands
  (defun use-package-normalize/:trigger-commands (name keyword args)
    (setq args (use-package-normalize-recursive-symlist name keyword args))
    (if (consp args) args (list args)))

  (defun use-package-handler/:trigger-commands (name _keyword arg rest state)
    (use-package-concat
     (cl-mapcan
      (lambda (command)
        (when (symbolp command)
          (unless (plist-get state :demand)
            `((satch-add-advice ',command :before (lambda () (require ',name)) nil :transient t)))))
      (delete-dups arg))
     (use-package-process-keywords name rest state)))

  ;; :pin-ref
  (defun use-package-normalize/:pin-ref (_name-symbol keyword args)
    (use-package-only-one (symbol-name keyword) args
      (lambda (_label arg)
        (cond ((stringp arg) arg)
              ((symbolp arg) (symbol-name arg))
              (t (use-package-error ":pin-ref wants a commit hash or a ref"))))))

  (defun use-package-handler/:pin-ref (name-symbol _keyword ref rest state)
    (let ((body (use-package-process-keywords name-symbol rest state)))
      (if (null ref)
          body
        `((let ((straight-current-profile 'pinned))
           (push '(,(symbol-name name-symbol) . ,ref) straight-x-pinned-packages)
           ,(macroexp-progn body))))))

  ;; HACK: This advice around `use-package' checks if a package is disabled in
  ;; `minemacs-disabled-packages' before calling `use-package'. This can come
  ;; handy if the user wants to enable some module while excluding some packages
  ;; from it. This advice also evaluates `use-package's conditional sections
  ;; (`:if', `:when' and `:unless') to prevent installing packages with
  ;; `straight'.
  (defun +use-package--check-if-disabled:around-a (origfn package &rest args)
    (if (or (+package-disabled-p package)
            (and (memq :if args)
                 (not (and (memq :if args) (eval (+varplist-get args :if t)))))
            (and (memq :when args)
                 (not (and (memq :when args) (eval (+varplist-get args :when t)))))
            (and (memq :unless args)
                 (not (and (memq :unless args) (not (eval (+varplist-get args :unless t)))))))
        ;; Register the package but don't enable it, useful when creating the lockfile,
        ;; this is the official straight.el way for conditionally installing packages
        (when-let* ((recipe (+varplist-get args :straight t)))
          (let* ((recipe (if (eq recipe t) (list package) recipe))
                 (car-recipe (and (listp recipe) (car recipe)))
                 (car-recipe-is-pkg (and (symbolp car-recipe) (not (keywordp car-recipe))))
                 (recipe (if (and car-recipe car-recipe-is-pkg) recipe (append (list package) recipe))))
            (straight-register-package recipe)))
      ;; Otherwise, add it to the list of configured packages and apply the `use-package' form
      (add-to-list 'minemacs-configured-packages package t)
      (apply origfn package args)))

  (advice-add 'use-package :around #'+use-package--check-if-disabled:around-a)

  ;; If you want to keep the `+use-package--check-if-disabled:around-a' advice after
  ;; loading MinEmacs' modules. You need to set in in your
  ;; "$MINEMACSDIR/early-config.el"
  (defvar +use-package-keep-checking-for-disabled-p nil)

  ;; The previous advice will be removed after loading MinEmacs packages to avoid
  ;; messing with the user configuration (for example, if the user manually
  ;; install a disabled package).
  (defun +use-package--remove-check-if-disabled-advice-h ()
    (unless +use-package-keep-checking-for-disabled-p
      (advice-remove 'use-package '+use-package--check-if-disabled:around-a)))

  (add-hook 'minemacs-after-loading-modules-hook #'+use-package--remove-check-if-disabled-advice-h))


(provide 'me-use-package-extra)

;;; me-use-package-extra.el ends here
