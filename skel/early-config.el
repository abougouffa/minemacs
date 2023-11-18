;;; early-config.el -*- coding: utf-8-unix; lexical-binding: t; -*-

;; This file will be loaded at the end of `early-init.el', it can be used to set
;; some early initialization stuff, or to set some MinEmacs variables, specially
;; these used in macros.

;; Set log level to `info' rather than `error'
(unless minemacs-verbose-p
  (setq minemacs-msg-level 2))

;; Disable `dashboard'
;; (setq +dashboard-disable t)

;; Enable full screen at startup
;; (if-let ((fullscreen (assq 'fullscreen default-frame-alist)))
;;     (setcdr fullscreen 'fullboth)
;;   (push '(fullscreen . fullboth) default-frame-alist))

;; Force loading lazy packages immediately, not in idle time
;; (setq minemacs-not-lazy-p t)

;; Setup a `debug-on-message' to catch a wired message!
;; (setq debug-on-message "Package cl is deprecated")

;; Compute statistics to use with `use-package-report'
;; (setq use-package-compute-statistics t)
