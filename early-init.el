;;; early-init.el -*- lexical-binding: t; -*-

(setq
 ;; Garbage collection tweaks: Increase the GC threshold for faster startup This
 ;; will be overriten when `gcmh' (Garbage Collector Magic Hack) is loaded.
 gc-cons-threshold most-positive-fixnum
 ;; Prefer loading newest compiled .el file
 load-prefer-newer t
 ;; Remove some unneeded UI elements
 default-frame-alist '((tool-bar-lines . 0)
                       (menu-bar-lines . 0)
                       (vertical-scroll-bars)
                       (mouse-color . "blue")
                       (left-fringe . 8)
                       (right-fringe . 13)
                       (fullscreen . maximized))
 ;; Set explicitly disabled modes
 tool-bar-mode nil
 menu-bar-mode nil
 scroll-bar-mode nil)

;; You can set the MINEMACS_ALPHA environment variable to an alpha percentage
(when (>= emacs-major-version 29)
  (when-let* ((alpha (getenv "MINEMACS_ALPHA"))
              (alpha (string-to-number alpha)))
    (push (cons 'alpha-background (if (zerop alpha) 93 alpha))
          default-frame-alist)))

;; For `lsp-mode' performance, set it here so we don't need to add it to the
;; system's environment variables.
(setenv "LSP_USE_PLISTS" "true")

;; Load MinEmacs variables first
(load (concat user-emacs-directory "core/me-vars.el") nil t)

;;; Load the early config file if it exists
(let ((early-config-path (concat minemacs-config-dir "early-config.el")))
  (when (file-exists-p early-config-path)
    (load early-config-path nil (not minemacs-verbose))))
