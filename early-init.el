;;; early-init.el -*- lexical-binding: t; -*-

;;; Garbage collection
;; Increase the GC threshold for faster startup
;; This will be overriten when `gcmh' is loaded
(setq gc-cons-threshold (* 1 1024 1024 1024)) ;; 1GB

;; Load MinEmacs variables first
(load (expand-file-name "core/me-vars.el" user-emacs-directory) nil :no-message)

;; Enable debugging on error when env variable "MINEMACS_DEBUG" is defined
(when minemacs-debug
  (setq debug-on-error t
        eval-expression-debug-on-error t))

;; Load environment variables when available
(let ((env-file (expand-file-name "env" minemacs-local-dir)))
  (when (file-exists-p env-file)
    (load env-file (not minemacs-verbose) (not minemacs-verbose))))

;;; Byte compilation
(setq byte-compile-warnings minemacs-verbose
      byte-compile-verbose minemacs-verbose)

;;; Native compilation settings
(when (featurep 'native-compile)
  ;; Silence compiler warnings as they can be pretty disruptive
  (setq native-comp-async-report-warnings-errors (when minemacs-verbose 'silent)
        native-comp-verbose (if minemacs-verbose 3 0))

  ;; Make native compilation happens asynchronously
  (if (version< emacs-version "29.0.50") ;; 29.1
      (setq native-comp-deferred-compilation t)
    (setq inhibit-automatic-native-compilation nil))

  ;; Set the right directory to store the native compilation cache
  ;; NOTE the method for setting the eln-cache directory depends on the emacs version
  (when (fboundp 'startup-redirect-eln-cache)
    (if (< emacs-major-version 29)
        (add-to-list
         'native-comp-eln-load-path
         (convert-standard-filename (expand-file-name "eln" minemacs-cache-dir)))
      (startup-redirect-eln-cache (convert-standard-filename (expand-file-name "eln" minemacs-cache-dir))))))

;; Add direcotries to `load-path'
(add-to-list 'load-path minemacs-core-dir)
(add-to-list 'load-path (expand-file-name "elisp" minemacs-root-dir))
(add-to-list 'load-path (expand-file-name "extras" minemacs-modules-dir))

;; Syncronization point!
;; Profile emacs startup and trigger `minemacs-loaded' 5s after loading Emacs
(add-hook
 'emacs-startup-hook
 (defun +minemacs--loaded-h ()
   (+log! "=============== Loaded Emacs ===============")
   (+info! "Loaded Emacs in %s." (emacs-init-time))

   (+log! "Applying `minemacs-fonts'.")
   ;; Load fonts, values are read from `minemacs-fonts' if set in config.el,
   ;; otherwise, they are read from the default `minemacs-default-fonts'.
   (+set-fonts)

   ;; Print load time, and a quote to *scratch*
   (with-current-buffer "*scratch*"
     (erase-buffer)
     (insert (format ";; Loaded MinEmacs in %s.\n" (emacs-init-time)))
     (insert ";; ==============================\n")
     (when (and (executable-find "fortune")
                (version<= "28.1" emacs-version)) ;; to use string-lines
       (insert (string-join
                (mapcar (lambda (l) (concat ";; " l))
                        (string-lines (shell-command-to-string "fortune")))
                "\n"))))

   ;; Require the virtual package to triggre loading packages depending on it
   (require 'minemacs-loaded)

   ;; Run hooks
   (when (boundp 'minemacs-after-startup-hook)
     (run-hooks 'minemacs-after-startup-hook))))

;;; Write user custom variables to separate file instead of init.el
(setq custom-file (expand-file-name "custom-vars.el" minemacs-config-dir))

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

;;; Load the early config file if it exists
(let ((early-config-path (expand-file-name "early-config.el" minemacs-config-dir)))
  (when (file-exists-p early-config-path)
    (load early-config-path nil 'nomessage)))
