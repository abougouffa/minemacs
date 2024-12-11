;;; early-config.el --- Early configuration file -*- coding: utf-8-unix; lexical-binding: t; -*-

;; Copyright (C) 2022-2024 Abdelhak Bougouffa

;; This file will be loaded at the end of `early-init.el', it can be used to set
;; some early initialization stuff, or to set some MinEmacs variables, specially
;; these used in macros.


;; Set log level to `info' rather than `error'
;; (unless minemacs-verbose-p
;;   (setq minemacs-msg-level 2))


;; Setup proxies
;; (setq minemacs-proxies
;;       '(("no" . "localhost,127.0.0.1,.local,.mylocaltld")
;;         ("ftp" . "http://myproxy.local:8080/")
;;         ("http" . "http://myproxy.local:8080/")
;;         ("https" . "http://myproxy.local:8080/")))


;; Enable full screen at startup
;; (if-let* ((fullscreen (assq 'fullscreen default-frame-alist)))
;;     (setcdr fullscreen 'fullboth)
;;   (push '(fullscreen . fullboth) default-frame-alist))


;; Frames can have a transparent background via the `alpha-background'
;; parameter. For better experience, this value should be set early before any
;; frame gets created (i.e. in "early-init.el"). This can be used to set the
;; alpha value to 95%.
;; (push '(alpha-background . 95) default-frame-alist)


;; Transparent title bar on MacOS!
;; (when (featurep 'ns) (push '(ns-transparent-titlebar . t) default-frame-alist))


;; Force loading lazy packages immediately, not in idle time
;; (setq minemacs-not-lazy-p t)


;; Setup a `debug-on-message' to catch a wired message!
;; (setq debug-on-message "Package cl is deprecated")


;; Compute statistics to use with `use-package-report'
;; (setq use-package-compute-statistics t)


;; Tell MinEmacs to use `straight' to update built-in packages
;; (setq minemacs-update-builtin-packages
;;       '(compat transient which-key tramp eglot org project editorconfig flymake xref eldoc))


;; Only load built-in packages. This can also be achieved by setting the
;; environment variable "MINEMACS_BUILTIN_ONLY"
;; (setq minemacs-builtin-only-p t)


;; Sometimes, a package gets loaded early at startup. This can be useful:
;; (defun +debug-on-require (feature)
;;   (advice-add 'require :before (lambda (f &rest _) (when (eq f feature) (debug)))))
;; (+debug-on-require 'parinfer-rust-flymake)
