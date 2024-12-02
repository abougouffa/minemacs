;;; early-init.el --- MinEmacs early initialization tweaks -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(add-to-list 'load-path (expand-file-name "core" (file-name-directory (file-truename (or load-file-name buffer-file-name)))))
(require 'me-vars)

(require 'me-lib)
(require 'package)
(require 'package-vc nil :noerror)

(setq
 ;; Avoid garbage collections during startup, this will be overwritten by `+minemacs--gc-tweaks-h'
 gc-cons-threshold most-positive-fixnum
 load-prefer-newer t
 package-user-dir (expand-file-name "elpa" minemacs-local-dir)
 package-quickstart t
 package-quickstart-file (expand-file-name "package-quickstart.el" minemacs-local-dir)
 package-vc-register-as-project nil
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
(add-hook
 'emacs-startup-hook
 (lambda ()
   (advice-remove 'tool-bar-setup #'ignore) ; Remove the advice so the toolbar can be enabled
   (when tool-bar-mode (tool-bar-setup)))) ; Ensure running it if toolbar is enabled

;; Frames can have a transparent background via the `alpha-background'
;; parameter. For better experience, this value should be set early before any
;; frame gets created (i.e. in "early-init.el"). MinEmacs uses the
;; `$MINEMACS_ALPHA` environment variable that can be set to an integer value in
;; the [1-100] range. When this variable is set but the value is not valid,
;; MinEmacs will fallback to the default alpha of 93%.
(when-let* ((alpha (getenv "MINEMACS_ALPHA"))
            (alpha (string-to-number alpha)))
  (add-to-list 'default-frame-alist `(alpha-background . ,(if (or (zerop alpha) (> alpha 100)) 93 alpha))))

(when (color-defined-p (+deserialize-sym 'minemacs--background-color nil t))
  (add-to-list 'default-frame-alist `(background-color . ,minemacs--background-color)))

;; Better titlebar on MacOS!
(when (featurep 'ns) (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

;; Load the user early configuration files
(+load-user-configs 'early-config 'local/early-config)

;;; early-init.el ends here
