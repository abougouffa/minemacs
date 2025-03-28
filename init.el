;; init.el --- MinEmacs core initialization file -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;     __  __ _         ______
;;    |  \/  (_)       |  ____|
;;    | \  / |_ _ __   | |__   _ __ ___   __ _  ___ ___
;;    | |\/| | | '_ \  |  __| | '_ ` _ \ / _` |/ __/ __|
;;    | |  | | | | | | | |____| | | | | | (_| | (__\__ \
;;    |_|  |_|_|_| |_| |______|_| |_| |_|\__,_|\___|___/
;;
;;  MINIMALIST & LIGHTWEIGHT EMACS CONFIGURATION FRAMEWORK
;;                           abougouffa.github.io/minemacs

;;; Commentary:

;; # MinEmacs - a minimalist & lightweight Emacs configuration framework

;; Load and hooks order:
;; - `~/.emacs.d/early-init.el`
;;   * `~/.emacs.d/core/me-lib.el`
;;     + `~/.emacs.d/core/me-vars.el`
;;   * `$MINEMACSDIR/early-config.el`           (unless disabled in `$MINEMACS_IGNORE_USER_CONFIG`)
;;   * `$MINEMACSDIR/local/early-config.el`     (unless disabled)
;; - `before-init-hook'
;; - `~/.emacs.d/init.el`
;;   * `~/.emacs.d/early-init.el`               (ensure it is loaded, in case we started Emacs without early-init)
;;   * `$MINEMACSDIR/custom-vars.el`
;;   * `~/.emacs.d/core/me-loaddefs.el`
;;   * `$MINEMACSDIR/init-tweaks.el`            (unless disabled)
;;   * `$MINEMACSDIR/local/init-tweaks.el`      (unless disabled)
;;   * `$MINEMACSDIR/local/modules.el`          (unless disabled)
;;   * `~/.emacs.d/core/me-builtin.el`
;;   * `~/.emacs.d/core/me-bootstrap.el`        (unless `MINEMACS_BUILTIN_ONLY`)
;;   * `~/.emacs.d/modules/<MODULE>.el`         (for <MODULE> in `minemacs-modules', unless `MINEMACS_BUILTIN_ONLY)
;;   * `minemacs-after-loading-modules-hook'
;;   * `$MINEMACSDIR/config.el`                 (unless disabled)
;;   * `$MINEMACSDIR/local/config.el`           (unless disabled)
;; - `after-init-hook'
;; - `emacs-startup-hook'
;;   * `minemacs-after-load-theme-hook'         (after applying `minemacs-theme')
;;   * `minemacs-after-startup-hook'
;;     + `minemacs-lazy-hook'                   (hooks are incrementally loaded via a timer)

;; Special hooks defined with `+make-first-file-hook!'
;; - `minemacs-first-file-hook'
;; - `minemacs-first-elisp-file-hook'
;; - `minemacs-first-python-file-hook'
;; - `minemacs-first-org-file-hook'
;; - `minemacs-first-c/c++-file-hook'

;;; Code:

;; Run a profiling session if `$MINEMACS_BENCHMARK' is defined.
(when (getenv "MINEMACS_BENCHMARK")
  (let ((dir (concat (file-name-directory load-file-name) "elisp/benchmark-init/")))
    (if (not (file-exists-p (concat dir "benchmark-init.el")))
        (error "[MinEmacs:Error] `benchmark-init' is not available, make sure you've run \"git submodule update --init\" inside MinEmacs' directory")
      (add-to-list 'load-path dir)
      (require 'benchmark-init)
      (benchmark-init/activate)
      (add-hook 'minemacs-lazy-hook (lambda () (benchmark-init/deactivate) (require 'benchmark-init-modes) (benchmark-init/show-durations-tree)) 99))))

(let ((min-ver 29) (recommended-ver 29)) ; Check if Emacs version is supported.
  (when (< emacs-major-version min-ver)
    (error "Emacs v%s is not supported, MinEmacs requires v%d or higher" emacs-version min-ver))
  (when (< emacs-major-version recommended-ver)
    (message "Recommended Emacs version for MinEmacs is %d or higher, you have v%s" recommended-ver emacs-version)))

(unless (featurep 'early-init) ; In case we decided to do some funny loading without the `early-init-file'
  (load (expand-file-name "early-init.el" (file-name-directory (file-truename load-file-name)))))

(require 'me-lib)

;; NOTE: It is important to set this here and not in `me-vars' nor in
;; "early-init.el", otherwise, it won't work with Chemacs2-based installations.
(setq user-emacs-directory minemacs-local-dir
      custom-file (concat minemacs-config-dir "custom-vars.el"))
(when (file-exists-p custom-file) (load custom-file))

(require 'me-lib) ; Load MinEmacs' core library

(setq
 ;; Enable debugging on error when Emacs if needed
 debug-on-error minemacs-debug-p
 ;; Decrease the warning type to `:error', unless we are running in verbose mode
 warning-minimum-level (if minemacs-verbose-p :warning :error)
 warning-minimum-log-level warning-minimum-level
 ;; Make byte compilation less noisy
 byte-compile-warnings minemacs-verbose-p
 byte-compile-verbose minemacs-verbose-p)

;; Native compilation settings
(when (and (featurep 'native-compile) (native-comp-available-p))
  (setq
   ;; Silence compiler warnings unless we are running in `minemacs-verbose-p' mode
   native-comp-async-report-warnings-errors (when minemacs-verbose-p 'silent)
   ;; Do not be too verbose
   native-comp-verbose (if minemacs-verbose-p 1 0)
   native-comp-debug (if minemacs-debug-p 1 0)
   ;; Make native compilation happens asynchronously
   native-comp-jit-compilation t)

  ;; Set the directory for storing the native compilation cache
  (startup-redirect-eln-cache (concat minemacs-cache-dir "eln/")))

(defun minemacs-generate-loaddefs ()
  "Generate MinEmacs' loaddefs file."
  (interactive)
  (when (file-exists-p minemacs-loaddefs-file) (delete-file minemacs-loaddefs-file))
  (loaddefs-generate
   (list minemacs-core-dir minemacs-elisp-dir minemacs-extras-dir minemacs-on-demand-modules-dir)
   minemacs-loaddefs-file))

;; Generate the loaddefs file if needed
(unless (file-exists-p minemacs-loaddefs-file) (minemacs-generate-loaddefs))

;; Then we load the loaddefs file
(+load minemacs-loaddefs-file)

;; Load user init tweaks when available
(+load-user-configs 'init-tweaks 'local/init-tweaks)

;; When `minemacs-proxies' is set in "early-config.el" or in "init-tweaks.el",
;; `minemacs-enable-proxy' will set the environment variables accordingly.
(unless minemacs-no-proxies-p (minemacs-enable-proxy minemacs-proxies))

;; HACK: Load the environment variables saved from shell using `+env-save' to
;; `+env-file'. `+env-save' saves all environment variables except these matched
;; by `+env-deny-vars'.
(+env-load) ; Load environment variables when available.

(defun +minemacs--loaded-h ()
  "This is MinEmacs' synchronization point.

To achieve fast startup, we try to defer loading most of the packages
until this hook is executed. This is managed by the `minemacs-loaded'
and `minemacs-lazy' features.

After loading Emacs, the `emacs-startup-hook' gets executed, we use this
hook to profile the startup time, and load the theme. Lastly we require
the `minemacs-loaded' synchronization module, which runs the
`minemacs-after-startup-hook' hooks and provide `minemacs-loaded' so the
packages loaded with `:after minemacs-loaded' can be loaded.

The `minemacs-loaded' will require `minemacs-lazy', which incrementally
run the hooks in `minemacs-lazy-hook' after startup, and at the end,
provide the `minemacs-lazy' feature so the packages loaded with `:after
minemacs-lazy' can be loaded."
  (+info! "Emacs%s loaded in %.3fs, including %.3fs for %d GCs." (if (daemonp) " (in daemon mode)" "")
          (float-time (time-subtract after-init-time before-init-time)) gc-elapsed gcs-done)
  (unless (featurep 'me-org-export-async-init) (+load-theme))
  (require 'minemacs-loaded))

;; Add it to the very beginning of `emacs-startup-hook'
(add-hook 'emacs-startup-hook #'+minemacs--loaded-h -91)

;; ========= Make some special hooks =========
(+make-first-file-hook! 'org "\\.org$")
(+make-first-file-hook! 'elisp "\\.elc?$")
(+make-first-file-hook! 'python (rx "." (or "py" "pyw" "pyx" "pyz" "pyzw") eol))
(+make-first-file-hook! 'c/c++ (rx "." (or "c" "cpp" "cxx" "cc" "c++" "h" "hpp" "hxx" "hh" "h++" "ixx" "cppm" "cxxm" "c++m" "ccm") eol))
(+make-first-file-hook! nil ".")

;; ========= Load MinEmacs packages and user customization =========
;; When running in an async Org export context, the used modules are set in
;; "modules/extras/me-org-export-async-init.el", so we must not override them
;; with the user's enabled modules.
(if (featurep 'me-org-export-async-init)
    (progn (message "Loading \"init.el\" in an org-export-async context.")
           ;; No need to load all modules, load only these related to Org
           (setq minemacs-modules '(me-org me-project me-prog me-emacs-lisp on-demand/me-latex)
                 minemacs-not-lazy-p t)) ; Don't be lazy
  ;; Load the default list of enabled modules `minemacs-modules'
  (+load-user-configs 'modules 'local/modules))

;; When the MINEMACS_LOAD_ALL_MODULES environment variable is set, we force
;; loading all modules, including on-demand ones.
(when minemacs-load-all-modules-p (setq minemacs-modules (minemacs-modules t)))

;; Load modules
(require 'me-builtin)
(require 'me-use-package-extra)
(require 'once)
(require 'satch)

(unless minemacs-builtin-only-p
  (require 'me-bootstrap)
  (mapc #'+load (mapcar (apply-partially #'format "%s%s.el" minemacs-modules-dir) minemacs-modules)))

(run-hooks 'minemacs-after-loading-modules-hook)
(+load-user-configs 'config 'local/config) ; Load user configuration

(+log! "Loaded init.el")

;;; init.el ends here
