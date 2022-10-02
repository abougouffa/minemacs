;;; early-init.el -*- lexical-binding: t; -*-

;;; Garbage collection
;; Increase the GC threshold for faster startup
(setq gc-cons-threshold (* 50 1000 1000))

;; Add direcotries to `load-path'
(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))

(load "vars.el" nil (not init-file-debug))
(load "utils.el" nil (not init-file-debug))

;;; Write user custom variables to separate file instead of init.el
(setq custom-file (expand-file-name "custom.el" minemacs-config-dir))

;;; Emacs lisp source/compiled preference
;; Prefer loading newest compiled .el file
(customize-set-variable 'load-prefer-newer noninteractive)

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
