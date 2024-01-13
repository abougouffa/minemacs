;;; early-init.el --- MinEmacs early initialization tweaks -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(setq
 ;; Do not make installed packages available when Emacs starts
 package-enable-at-startup nil
 ;; HACK: Increase the garbage collection (GC) threshold for faster startup.
 ;; This will be overwritten when `gcmh-mode' (a.k.a. the Garbage Collector
 ;; Magic Hack) gets loaded in the `me-gc' module (see "init.el").
 gc-cons-threshold most-positive-fixnum
 ;; Prefer loading newer files
 load-prefer-newer t
 ;; Remove some unneeded UI elements
 default-frame-alist '((tool-bar-lines . 0)
                       (menu-bar-lines . 0)
                       (vertical-scroll-bars)
                       (mouse-color . "blue")
                       (left-fringe . 8)
                       (right-fringe . 13)
                       (internal-border-width . 10)
                       (fullscreen . maximized))
 ;; Explicitly set modes disabled in `default-frame-alist' to nil
 tool-bar-mode nil
 menu-bar-mode nil
 scroll-bar-mode nil)

;; NOTE: In Emacs29+, frames can have a transparent background via the
;; `alpha-background' parameter. For a better experience, this value should be
;; set early before any frame gets created (i.e. in "early-init.el"). MinEmacs
;; uses the `$MINEMACS_ALPHA` environment variable that can be set to an integer
;; value in the [1-100] range (the alpha percentage). When this variable is not
;; set, Emacs will load the default GUI (without background alpha), and when it
;; is set but the value is not valid, MinEmacs will fallback to the default
;; alpha of 93%.
(when (>= emacs-major-version 29)
  (when-let* ((alpha (getenv "MINEMACS_ALPHA"))
              (alpha (string-to-number alpha)))
    (push (cons 'alpha-background (if (or (zerop alpha) (> alpha 100)) 93 alpha))
          default-frame-alist)))

;; HACK: In `lsp-mode' (see the `me-lsp' module), the user can define the
;; `$LSP_USE_PLISTS=true` to improve `lsp-mode' performances. We set this
;; environment variable here so we don't need to add it to the system's
;; environment variables.
(setenv "LSP_USE_PLISTS" "true")

;; Load MinEmacs variables from the `me-vars' core module.
(load (expand-file-name "core/me-vars.el" (file-name-directory (file-truename load-file-name))) nil t)

;; Better titlebar on MacOS!
(when (and os/mac (featurep 'ns))
  (push '(ns-transparent-titlebar . t) default-frame-alist))

;; Load the user early configuration files
(+load-user-configs 'early-config 'local/early-config)

;;; early-init.el ends here
