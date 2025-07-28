;;; early-init.el --- MinEmacs early initialization tweaks -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2022-09-17
;; Last modified: 2025-07-28

;;; Commentary:

;;; Code:

(let ((min-ver 30) (recommended-ver 31)) ; Check if Emacs version is supported.
  (when (< emacs-major-version min-ver)
    (error "Emacs v%s is not supported, MinEmacs requires v%d or higher" emacs-version min-ver))
  (when (< emacs-major-version recommended-ver)
    (message "Recommended Emacs version for MinEmacs is %d or higher, you have v%s" recommended-ver emacs-version)))

(let ((minemacs-dir (file-name-directory (file-truename load-file-name))))
  (when-let* ((val (getenv "MINEMACS_BENCHMARK"))) ; Run a profiling session if `$MINEMACS_BENCHMARK' is defined
    (add-to-list 'load-path (expand-file-name "elisp/benchmark-init/" minemacs-dir))
    (if (not (require 'benchmark-init nil :noerror))
        (error "[MinEmacs:Error] `benchmark-init' is not available, make sure you've run \"git submodule update --init\" inside MinEmacs' directory")
      (benchmark-init/activate)
      (add-hook (if (equal (downcase val) "lazy") 'minemacs-lazy-hook 'minemacs-after-startup-hook)
                (lambda () (benchmark-init/deactivate) (require 'benchmark-init-modes) (benchmark-init/show-durations-tree)) 99)))

  (dolist (dir '("core" "core/extras" "modules" "modules/extras" "elisp")) ; Add some of MinEmacs' directories to `load-path'
    (add-to-list 'load-path (expand-file-name dir minemacs-dir))))

(require 'me-vars)
(require 'me-lib)

;; Avoid spitting messages like "somefile.el: Warning: ‘if-let’ is an obsolete macro (as of 31.1); use ‘if-let*’ instead."
(unless minemacs-verbose-p
  (advice-add 'macroexp-warn-and-return :around '+apply-inhibit-messages))

(setq
 ;; Enable debugging on error when Emacs if needed
 debug-on-error minemacs-debug-p
 ;; Decrease the warning type to `:error', unless we are running in verbose mode
 warning-minimum-level (if minemacs-verbose-p :warning :error)
 warning-minimum-log-level warning-minimum-level
 ;; Enable EIEIO backward compatibility and don't warn about it
 eieio-backward-compatibility t
 ;; Make byte compilation less noisy
 byte-compile-warnings minemacs-verbose-p
 byte-compile-verbose minemacs-verbose-p
 ;; Set `use-package' to verbose when MinEmacs is started in verbose mode
 use-package-verbose (cond (minemacs-debug-p 'debug) (minemacs-verbose-p t))
 ;; Defer loading packages by default, use `:demand' to force loading a package
 use-package-always-defer (not minemacs-always-demand-p)
 use-package-always-demand minemacs-always-demand-p
 ;; Make the expanded code as minimal as possible, do not try to catch errors
 use-package-expand-minimally (not minemacs-debug-p)
 ;; Do not make installed packages available when Emacs starts (we use `straight')
 package-enable-at-startup nil
 ;; Better garbage collection settings, no GCMH required, See: https://zenodo.org/records/10518083
 gc-cons-threshold (* 100 1000 1000)
 gc-cons-percentage 0.2
 ;; Prefer loading newer files
 load-prefer-newer t
 ;; Remove some unneeded UI elements
 default-frame-alist '((tool-bar-lines . 0)
                       (menu-bar-lines . 0)
                       (vertical-scroll-bars)
                       (left-fringe . 8)
                       (right-fringe . 13)
                       (internal-border-width . 5)
                       (fullscreen . maximized))
 ;; Explicitly set modes disabled in `default-frame-alist' to nil
 tool-bar-mode nil
 menu-bar-mode nil
 scroll-bar-mode nil
 ;; Set mode-line format to prevent it from showing at startup
 mode-line-format nil)

;; Native compilation settings
(when (and (featurep 'native-compile) (native-comp-available-p))
  (setq
   ;; Silence compiler warnings unless we are running in `minemacs-verbose-p' mode
   native-comp-async-report-warnings-errors (when minemacs-verbose-p 'silent)
   native-comp-verbose (if minemacs-verbose-p 1 0)
   native-comp-debug (if minemacs-debug-p 1 0)
   native-comp-jit-compilation t ; Make native compilation happens asynchronously
   native-comp-async-query-on-exit t) ; Ask before terminating asynchronous compilations on exit
  ;; Set the directory for storing the native compilation cache
  (startup-redirect-eln-cache (concat minemacs-cache-dir "eln/")))

;; PERF: Setting `file-name-handler-alist' to nil should boost startup time.
;; https://reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start
;; Store the current value so we can reset it after Emacs startup.
(put 'file-name-handler-alist 'original-value (default-toplevel-value 'file-name-handler-alist))
(set-default-toplevel-value 'file-name-handler-alist nil) ; Make sure the new value survives any current let-binding
;; Restore `file-name-handler-alist' after startup while conserving the potential new elements
(add-hook 'emacs-startup-hook (lambda () (setq file-name-handler-alist (delete-dups (append file-name-handler-alist (get 'file-name-handler-alist 'original-value))))) 99)

;; HACK: This advice around `use-package' checks if a package is disabled in
;; `minemacs-disabled-packages' before calling `use-package'.
(defun +use-package--check-if-disabled:around-a (origfn package &rest args)
  (unless (+package-disabled-p package)
    (add-to-list 'minemacs-configured-packages package t)
    (apply origfn package args)))

(advice-add 'use-package :around '+use-package--check-if-disabled:around-a)

(defcustom +use-package-keep-checking-for-disabled-p nil
  "If you want to keep the advice that skip disabled packages.
You need to set in in your \"early-config.el\"."
  :group 'minemacs :type 'boolean)

;; Load the user early configuration files
(+load-user-configs 'early-config 'local/early-config)

(provide 'early-init)
;;; early-init.el ends here
