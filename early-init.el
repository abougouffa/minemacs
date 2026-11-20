;;; early-init.el --- MinEmacs early initialization tweaks -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2022-09-17
;; Last modified: 2026-11-20

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

;; Set before the early activation below (ex. `package-user-dir')
(setq
 ;; Only activate the declared packages (see `+package-ensure' in "me-bootstrap.el")
 package-enable-at-startup nil
 ;; Install packages in MinEmacs' local directory
 package-user-dir (concat minemacs-local-dir "elpa/")
 ;; Package archives, the user can change these in "early-config.el" or "init-tweaks.el"
 package-archives '(("melpa" . "https://melpa.org/packages/")
                    ("gnu" . "https://elpa.gnu.org/packages/")
                    ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                    ("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/"))
 ;; Prefer MELPA (latest commits, like `straight' which used MELPA recipes first)
 package-archive-priorities '(("melpa" . 30) ("gnu" . 20) ("nongnu" . 10) ("jcs-elpa" . 10))
 ;; Install the latest commit for packages installed from VC with `:vc', like `straight'
 use-package-vc-prefer-newest t
 ;; Don't register the cloned packages as projects
 package-vc-register-as-project nil)

;; Activate the installed newer versions of built-in packages (ex. `transient',
;; `compat') before anything loads the built-in ones. Otherwise, the built-in
;; version stays in use (Emacs 30) or gets reloaded with an "activated too
;; late" warning (Emacs 31+). The other packages are activated when declared.
(unless minemacs-builtin-only-p
  (require 'package)
  (package-load-all-descriptors) ; Load the installed packages descriptors, without activating them
  (dolist (package (mapcar #'car package-alist))
    (when (assq package package--builtin-versions)
      (with-demoted-errors "Error activating package: %S"
        (package-activate package)))))

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
 use-package-expand-minimally (not minemacs-debug-p) ; Minimize expanded code & do not try to catch errors, unless in debug mode
 ;; Prefer loading newer files
 load-prefer-newer t
 ;; Remove some unneeded UI elements
 default-frame-alist '((tool-bar-lines . 0)
                       (menu-bar-lines . 0)
                       (vertical-scroll-bars)
                       (height . 50)
                       (width . 80)
                       (left-fringe . 8)
                       (right-fringe . 13)
                       (internal-border-width . 5)
                       (fullscreen . maximized))
 ;; Explicitly set modes disabled in `default-frame-alist' above
 tool-bar-mode nil
 menu-bar-mode nil
 scroll-bar-mode nil
 ;; Set mode-line format to prevent it from showing at startup
 mode-line-format nil)

;; Better garbage collection settings, see: https://zenodo.org/records/10518083
(unless (fboundp 'igc-info) ; No need when Emacs is built with incremental GC
  (setq gc-cons-threshold (* 100 1000 1000)
        gc-cons-percentage 0.2))

;; Native compilation settings
(when (and (featurep 'native-compile) (native-comp-available-p))
  (setq native-comp-async-report-warnings-errors (when minemacs-verbose-p 'silent) ; Silent reporting in `minemacs-verbose-p' mode, none otherwise
        native-comp-verbose (if minemacs-verbose-p 1 0)
        native-comp-debug (if minemacs-debug-p 1 0)
        native-comp-jit-compilation t ; Make native compilation happens asynchronously
        package-native-compile t ; Natively compile packages ahead-of-time when installing/upgrading them
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

;; HACK: Skip the `use-package' declarations of the packages disabled in
;; `minemacs-disabled-packages' (or by their `:disabled', `:if', `:when' and
;; `:unless' conditions) and register the other ones in
;; `minemacs-configured-packages'. This is done while loading MinEmacs' modules,
;; see `+use-package-check-for-disabled'.
(advice-add
 'use-package :around
 (+defun +use-package--check-if-disabled:around-a (origfn package &rest args)
   (if (not +use-package-check-for-disabled)
       (apply origfn package args)
     (unless (or (+package-disabled-p package)
                 (memq :disabled args)
                 (and (memq :if args) (not (eval (+varplist-get args :if t))))
                 (and (memq :when args) (not (eval (+varplist-get args :when t))))
                 (and (memq :unless args) (eval (+varplist-get args :unless t))))
       (add-to-list 'minemacs-configured-packages package t)
       (apply origfn package args)))))

;; Load the user early configuration files
(+load-user-configs 'early-config 'local/early-config)

;; Useful if I set `debug-on-error' in "local/early-config.el"
(when debug-on-error (setq minemacs-debug-p t))

(provide 'early-init)
;;; early-init.el ends here
