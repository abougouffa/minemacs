;;; early-init.el --- MinEmacs early initialization tweaks -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(setq
 ;; Do not make installed packages available when Emacs starts (we use `straight')
 package-enable-at-startup nil
 ;; Avoid garbage collections during startup, this will be overwritten by `+minemacs--gc-tweaks-h'
 gc-cons-threshold most-positive-fixnum
 ;; Prefer loading newer files
 load-prefer-newer t
 ;; Remove some unneeded UI elements
 default-frame-alist '((tool-bar-lines . 0)
                       (menu-bar-lines . 0)
                       (vertical-scroll-bars)
                       (left-fringe . 8)
                       (right-fringe . 13)
                       (internal-border-width . 15)
                       (mouse-color . "blue")
                       (fullscreen . maximized))
 ;; Explicitly set modes disabled in `default-frame-alist' to nil
 tool-bar-mode nil
 menu-bar-mode nil
 scroll-bar-mode nil
 ;; Set mode-line format to prevent it from showing at startup
 mode-line-format nil)

;; It seems like, even when `tool-bar-mode' is nil, `tool-bar-setup' still be called
(advice-add 'tool-bar-setup :override #'ignore)

;; We restore it after starting Emacs so the user can manually enable the `tool-bar'
(add-hook 'emacs-startup-hook (lambda () (advice-remove 'tool-bar-setup #'ignore)))

;; Frames can have a transparent background via the `alpha-background'
;; parameter. For better experience, this value should be set early before any
;; frame gets created (i.e. in "early-init.el"). MinEmacs uses the
;; `$MINEMACS_ALPHA` environment variable that can be set to an integer value in
;; the [1-100] range. When this variable is set but the value is not valid,
;; MinEmacs will fallback to the default alpha of 93%.
(when-let* ((alpha (getenv "MINEMACS_ALPHA"))
            (alpha (string-to-number alpha)))
  (push `(alpha-background . ,(if (or (zerop alpha) (> alpha 100)) 93 alpha)) default-frame-alist))

;; Load MinEmacs variables from the `me-vars' core module.
(load (expand-file-name "core/me-vars.el" (file-name-directory (file-truename load-file-name))) nil t)

;; Better titlebar on MacOS!
(when (and os/mac (featurep 'ns))
  (push '(ns-transparent-titlebar . t) default-frame-alist))

;; Load the user early configuration files
(+load-user-configs 'early-config 'local/early-config)

;;; early-init.el ends here
