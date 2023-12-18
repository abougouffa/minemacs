;; me-use-package-extra.el --- Extend use-package to allow straight-x package pinning -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;; Add support for pinning versions of individual packages. See:
;; github.com/radian-software/straight.el#how-do-i-pin-package-versions-or-use-only-tagged-releases

;;; Code:

;; Allow pinning versions from `use-package' using the `:pin-ref' keyword
(with-eval-after-load 'use-package-core
  ;; HACK: This advice around `use-package' checks if a package is disabled in
  ;; `minemacs-disabled-packages' before calling `use-package'. This can come
  ;; handy if the user wants to enable some module while excluding some packages
  ;; from it.
  (advice-add
   'use-package :around
   (defun +use-package--check-if-disabled-a (origfn package &rest args)
     (when (not (+package-disabled-p package))
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
       (advice-remove 'use-package '+use-package--check-if-disabled-a))))

  ;; Make `:straight' an alias to `:elpaca' to provide a minimal backward
  ;; compatibility
  (add-to-list 'use-package-keywords :straight)
  (defalias 'use-package-normalize/:straight 'use-package-normalize/:elpaca)
  (defalias 'use-package-handler/:straight 'use-package-handler/:elpaca))


(provide 'me-use-package-extra)

;;; me-use-package-extra.el ends here
