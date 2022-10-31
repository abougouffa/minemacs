;;; early-init.el -*- lexical-binding: t; -*-

;;; Garbage collection
;; Increase the GC threshold for faster startup
;; This will be overriten when `gcmh' is loaded
(setq gc-cons-threshold (* 1 1024 1024 1024)) ;; 1GB

;;; Emacs lisp source/compiled preference
;; Prefer loading newest compiled .el file
(customize-set-variable 'load-prefer-newer noninteractive)

;;; UI configuration
;; Remove some unneeded UI elements (the user can turn back on anything they wish)
(setq inhibit-startup-message t)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(mouse-color . "blue") default-frame-alist)
(push '(left-fringe . 8)  default-frame-alist)
(push '(right-fringe . 8) default-frame-alist)

(when (and (>= emacs-major-version 29) nil)
  (push '(alpha-background . 90) default-frame-alist))

(setq tool-bar-mode nil
      menu-bar-mode nil
      scroll-bar-mode nil)

;; Make the initial buffer load faster by setting its mode to fundamental-mode
(customize-set-variable 'initial-major-mode 'fundamental-mode)
