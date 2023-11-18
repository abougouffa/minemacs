;; me-use-package-extra.el --- Extend use-package to allow straight-x package pinning -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;; Add support for pinning versions of individual packages. See:
;; github.com/radian-software/straight.el#how-do-i-pin-package-versions-or-use-only-tagged-releases

;;; Code:

(with-eval-after-load 'straight
  ;; Add a profile (and lockfile) for stable package revisions.
  (add-to-list 'straight-profiles '(pinned . "pinned.el"))
  (require 'straight-x))

;; Allow pinning versions from `use-package' using the `:pin-ref' keyword
(with-eval-after-load 'use-package-core
  (add-to-list 'use-package-keywords :pin-ref)

  (defun use-package-normalize/:pin-ref (_name-symbol keyword args)
    (use-package-only-one (symbol-name keyword) args
      (lambda (_label arg)
        (cond ((stringp arg) arg)
              ((symbolp arg) (symbol-name arg))
              (t (use-package-error ":pin-ref wants a commit hash or a ref."))))))

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
  (advice-add
   'use-package :around
   (defun +use-package--check-if-disabled-a (origfn package &rest args)
     (when (and (not (+package-disabled-p package))
                (or (not (memq :if args))
                    (and (memq :if args) (eval (+varplist-get args :if t))))
                (or (not (memq :when args))
                    (and (memq :when args) (eval (+varplist-get args :when t))))
                (or (not (memq :unless args))
                    (and (memq :unless args) (not (eval (+varplist-get args :unless t))))))
       (add-to-list 'minemacs-configured-packages package t)
       (apply origfn package args))))

  ;; If you want to keep the `+use-package--check-if-disabled-a' advice after
  ;; loading MinEmacs' modules. You need to set in in your
  ;; "$MINEMACSDIR/early-config.el"
  (defvar +use-package-keep-checking-for-disabled-p nil)

  ;; The previous advice will be removed after loading MinEmacs packages to avoid
  ;; messing with the user configuration (for example, if the user manually
  ;; install a disabled package).
  (add-hook
   'minemacs-after-loading-modules-hook
   (defun +use-package--remove-check-if-disabled-advice-h ()
     (unless +use-package-keep-checking-for-disabled-p
       (advice-remove 'use-package '+use-package--check-if-disabled-a)))))


(provide 'me-use-package-extra)

;;; me-use-package-extra.el ends here
