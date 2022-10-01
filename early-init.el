;;; early-init.el -*- lexical-binding: t; -*-

;;; Garbage collection
;; Increase the GC threshold for faster startup
(setq gc-cons-threshold (* 50 1000 1000))

;;; Emacs lisp source/compiled preference
;; Prefer loading newest compiled .el file
(customize-set-variable 'load-prefer-newer noninteractive)

(defvar minemacs-config-dir (or (getenv "MINEMACS_DIR")
                                (when (file-directory-p user-emacs-directory)
                                  user-emacs-directory)
                                (expand-file-name "~/.minemacs.d/")))

(defvar minemacs-etc-dir (expand-file-name "etc/" user-emacs-directory))
(defvar minemacs-var-dir (expand-file-name "var/" user-emacs-directory))
(defvar minemacs-cache-dir (expand-file-name "cache/" minemacs-var-dir))

(add-to-list 'load-path (expand-file-name minemacs-config-dir))

(unless (file-exists-p minemacs-config-dir)
  (mkdir minemacs-config-dir t))

;;; Native compilation settings
(when (featurep 'native-compile)
  ;; Silence compiler warnings as they can be pretty disruptive
  (setq native-comp-async-report-warnings-errors nil)

  ;; Make native compilation happens asynchronously
  (setq native-comp-deferred-compilation t)

  ;; Set the right directory to store the native compilation cache
  ;; NOTE the method for setting the eln-cache directory depends on the emacs version
  (when (fboundp 'startup-redirect-eln-cache)
    (if (version< emacs-version "29")
        (add-to-list 'native-comp-eln-load-path
                     (convert-standard-filename (expand-file-name "eln" minemacs-cache-dir)))
      (startup-redirect-eln-cache (convert-standard-filename (expand-file-name "eln" minemacs-cache-dir)))))

  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln" minemacs-cache-dir)))

;;; UI configuration
;; Remove some unneeded UI elements (the user can turn back on anything they wish)
(setq inhibit-startup-message t)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(mouse-color . "blue") default-frame-alist)

;; Make the initial buffer load faster by setting its mode to fundamental-mode
(customize-set-variable 'initial-major-mode 'fundamental-mode)

;;; Load the early config file if it exists
(let ((early-config-path (expand-file-name "early-config.el" minemacs-config-dir)))
  (when (file-exists-p early-config-path)
    (load early-config-path nil 'nomessage)))
