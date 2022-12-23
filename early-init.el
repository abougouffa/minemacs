;;; early-init.el -*- lexical-binding: t; -*-

(setq
 ;; Garbage collection
 ;; Increase the GC threshold for faster startup
 ;; This will be overriten when `gcmh' is loaded
 gc-cons-threshold most-positive-fixnum
 ;; Prefer loading newest compiled .el file
 load-prefer-newer t)

;; Remove some unneeded UI elements
(setq default-frame-alist
      '((tool-bar-lines . 0)
        (menu-bar-lines . 0)
        (vertical-scroll-bars)
        (mouse-color . "blue")
        (left-fringe . 8)
        (right-fringe . 8)))

(when (and (>= emacs-major-version 29) (getenv "MINEMACS_ALPHA"))
  (push '(alpha-background . 93) default-frame-alist))

(setq tool-bar-mode nil
      menu-bar-mode nil
      scroll-bar-mode nil)

;; Load MinEmacs variables first
(load (concat user-emacs-directory "core/me-vars.el") nil t)

;;; Load the early config file if it exists
(let ((early-config-path (concat minemacs-config-dir "early-config.el")))
  (when (file-exists-p early-config-path)
    (load early-config-path nil (not minemacs-verbose))))
