;; init.el --- MinEmacs core initialization file -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

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
;; - `$MINEMACSDIR/early-config.el` (unless disabled in `$MINEMACS_IGNORE_USER_CONFIG`)
;; - `$MINEMACSDIR/local/early-config.el` (unless disabled)
;; - `~/.emacs.d/init.el`
;;   * `before-init-hook'
;;   * `~/.emacs.d/core/me-vars.el`
;;   * `~/.emacs.d/core/backports/*.el` (when Emacs < 29)
;;   * `~/.emacs.d/core/me-loaddefs.el`
;;   * `$MINEMACSDIR/init-tweaks.el` (unless disabled)
;;   * `$MINEMACSDIR/local/init-tweaks.el` (unless disabled)
;;   * `$MINEMACSDIR/modules.el` (unless disabled)
;;   * `$MINEMACSDIR/local/modules.el` (unless disabled)
;;   * `~/.emacs.d/core/<module>.el`
;;   * `~/.emacs.d/modules/<module>.el` (for module in `minemacs-modules')
;;   * `minemacs-after-loading-modules-hook'
;;   * `$MINEMACSDIR/custom-vars.el`
;;   * `$MINEMACSDIR/config.el` (unless disabled)
;;   * `$MINEMACSDIR/local/config.el` (unless disabled)
;;   * `after-init-hook'
;;   * `emacs-startup-hook'
;;   * `minemacs-after-startup-hook'
;;     + `minemacs-lazy-hook' (delayed)

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

      (defun +benchmark-init--desactivate-and-show-h ()
        (benchmark-init/deactivate)
        (require 'benchmark-init-modes)
        (benchmark-init/show-durations-tree))

      (with-eval-after-load 'me-vars
        (add-hook 'minemacs-lazy-hook #'+benchmark-init--desactivate-and-show-h 99)))))

;; Check if Emacs version is supported.
(let ((min-ver 28))
  (when (< emacs-major-version min-ver)
    (error "Emacs v%s is not supported, MinEmacs requires v%s or higher" emacs-version min-ver)))

;; PERF: Setting `file-name-handler-alist' to nil should boost startup time.
;; reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start
;; Store the current value so we can reset it after Emacs startup.
(put 'file-name-handler-alist 'original-value (default-toplevel-value 'file-name-handler-alist))
;; Make sure the new value survives any current let-binding.
(set-default-toplevel-value 'file-name-handler-alist nil)
;; After Emacs startup, we restore `file-name-handler-alist' while conserving
;; the potential new elements made during startup.
(defun +mineamcs--restore-file-name-handler-alist-h ()
  (setq file-name-handler-alist (delete-dups (append file-name-handler-alist (get 'file-name-handler-alist 'original-value)))))
(add-hook 'emacs-startup-hook '+mineamcs--restore-file-name-handler-alist-h 100)

;; HACK: At this point, MinEmacs variables defined in `me-vars' should be
;; already loaded (in "early-init.el"). However, we double-check here and load
;; them if necessary in case Emacs has been loaded directly from "init.el"
;; without passing by "early-init.el". This can happen when we are running in a
;; `me-org-export-async-init' context, or if we use some bootstrapping mechanism
;; like Chemacs2.
(unless (featurep 'me-vars)
  (load (expand-file-name "core/me-vars.el" (file-name-directory (file-truename load-file-name))) nil t))

;; Add some of MinEmacs' directories to `load-path'.
(setq load-path (append (list minemacs-core-dir minemacs-elisp-dir minemacs-extras-dir minemacs-modules-dir) load-path))

;; Load MinEmacs' library
(require 'me-lib)

;; HACK: Most Emacs' builtin and third-party packages depends on the
;; `user-emacs-directory' variable to store cache information, generated
;; configuration files and downloaded utilities. However, this will mess with
;; MinEmacs' directory (which defaults to `user-emacs-directory'). To keep the
;; "~/.emacs.d/" directory clean, we overwrite the `user-emacs-directory' at
;; early stage with `minemacs-local-dir' so all generated files gets stored in
;; "~/.emacs.d/local/".
;; NOTE: It is important to set this here and not in `me-vars' nor in
;; "early-init.el", otherwise, it won't work with Chemacs2-based installations.
(setq user-emacs-directory minemacs-local-dir)

;; HACK: Load Emacs 29 back ports for earlier Emacs versions. Note that I do
;; only back port a very small number of the functions/variables that I use at
;; early stage from Emacs29+ to be compatible with Emacs 28.2. For any Emacs
;; version less than 29, MinEmacs will enable the `me-compat' module and load it
;; just after `me-bootstrap'. This module loads the `compat' package which
;; provide several forward compatibility functions, it is loaded at an early
;; stage to provide its functionality to the rest of the modules so we can use
;; some new features when configuring them.
(when (< emacs-major-version 29)
  (let ((backports-dir (concat minemacs-core-dir "backports/")))
    (mapc (apply-partially #'+load backports-dir) (directory-files backports-dir nil "\\.el\\'"))))

(setq
 ;; Enable debugging on error when Emacs is launched with the `--debug-init`
 ;; option or when the environment variable `$MINEMACS_DEBUG` is defined (see
 ;; `me-vars').
 debug-on-error minemacs-debug-p
 ;; Decrease the warning type to `:error', unless we are running in verbose mode
 warning-minimum-level (if minemacs-verbose-p :warning :error)
 warning-minimum-log-level warning-minimum-level
 ;; Make byte compilation less noisy
 byte-compile-warnings minemacs-verbose-p
 byte-compile-verbose minemacs-verbose-p)

;; Native compilation settings
(when (featurep 'native-compile)
  (setq
   ;; Silence compiler warnings as they can be pretty disruptive, unless we are
   ;; running in `minemacs-verbose-p' mode.
   native-comp-async-report-warnings-errors (when minemacs-verbose-p 'silent)
   native-comp-verbose (if minemacs-verbose-p 1 0) ; do not be too verbose
   native-comp-debug (if minemacs-debug-p 1 0)
   ;; Make native compilation happens asynchronously.
   native-comp-jit-compilation t)

  ;; Set the right directory to store the native compilation cache to avoid
  ;; messing with "~/.emacs.d/".
  (startup-redirect-eln-cache (concat minemacs-cache-dir "eln/")))

(defun minemacs-generate-loaddefs ()
  "Generate MinEmacs' loaddefs file."
  (interactive)
  (when (file-exists-p minemacs-loaddefs-file) (delete-file minemacs-loaddefs-file))
  (apply (if (fboundp 'loaddefs-generate) #'loaddefs-generate #'make-directory-autoloads)
         (list (list minemacs-core-dir minemacs-elisp-dir minemacs-extras-dir) minemacs-loaddefs-file)))

;; Some of MinEmacs commands and libraries are defined to be auto-loaded. In
;; particular, these in the `minemacs-core-dir', `minemacs-elisp-dir', and
;; `minemacs-extras-dir' directories. The generated loaddefs file will be stored
;; in `minemacs-loaddefs-file'. We first regenerate the loaddefs file if it
;; doesn't exist.
(unless (file-exists-p minemacs-loaddefs-file) (minemacs-generate-loaddefs))

;; Then we load the loaddefs file
(+load minemacs-loaddefs-file)

;; Load user init tweaks when available
(+load-user-configs 'init-tweaks 'local/init-tweaks)

;; When `minemacs-proxies' is set in "early-init.el" or in "init-tweaks.el",
;; `minemacs-enable-proxy' will set the environment variables accordingly.
(unless minemacs-no-proxies-p (minemacs-enable-proxy minemacs-proxies))

;; HACK: Load the environment variables saved from shell using `+env-save' to
;; `+env-file'. `+env-save' saves all environment variables except these matched
;; by `+env-deny-vars'.
(+env-load) ; Load environment variables when available.

(defun +minemacs--loaded-h ()
  "This is MinEmacs' synchronization point.

To achieve fast Emacs startup, we tries to defer loading most of the packages
until this hook is executed. This is managed by the `minemacs-loaded' and
`minemacs-lazy' features.

After loading Emacs, the `emacs-startup-hook' gets executed, we use this hook to
profile the startup time, load the theme, and make a persistent scratch buffer.
Lastly we require the `minemacs-loaded' synchronization module, which runs
the `minemacs-after-startup-hook' hooks and provide `minemacs-loaded' so the
packages loaded with `:after minemacs-loaded' can be loaded.

The `minemacs-loaded' will require `minemacs-lazy' when Emacs goes idle, this
provides `minemacs-lazy' so the packages loaded with `:after minemacs-lazy' can
be loaded then it incrementally run the hooks in `minemacs-lazy-hook' when Emacs
goes idle."
  (+info! "Loaded Emacs%s in %s, including %.3fs for %d GCs." (if (daemonp) " (in daemon mode)" "") (emacs-init-time) gc-elapsed gcs-done)
  (unless (featurep 'me-org-export-async-init) (+load-theme))
  (require 'minemacs-loaded))

;; Add it to the very beginning of `emacs-startup-hook'
(add-hook 'emacs-startup-hook #'+minemacs--loaded-h -101)

;; ========= Make some special hooks =========
(+make-first-file-hook! 'org "\\.org$")
(+make-first-file-hook! 'elisp "\\.elc?$")
(+make-first-file-hook! 'python (rx "." (or "py" "pyw" "pyx" "pyz" "pyzw") eol))
(+make-first-file-hook! 'c/c++ (rx "." (or "c" "cpp" "cxx" "cc" "c++" "h" "hpp" "hxx" "hh" "h++" "ixx" "cppm" "cxxm" "c++m" "ccm") eol))
(+make-first-file-hook! nil ".")

;; ========= Load MinEmacs packages and user customization =========
;; When running in an async Org export context, the used modules are set in
;; modules/extras/me-org-export-async-init.el, so we must not override them with
;; the user's enabled modules.
(if (featurep 'me-org-export-async-init)
    (progn (message "Loading \"init.el\" in an org-export-async context.")
           (setq minemacs-not-lazy-p t))
  ;; Load the default list of enabled modules `minemacs-modules'
  (+load minemacs-core-dir "me-modules.el")
  (+load-user-configs 'modules 'local/modules))

;; When the MINEMACS_LOAD_ALL_MODULES environment variable is set, we force
;; loading all modules.
(when minemacs-load-all-modules-p
  (setq minemacs-modules (mapcar #'intern (mapcar #'file-name-sans-extension (directory-files minemacs-modules-dir nil "\\`me-.*\\.el\\'")))))

(when (bound-and-true-p minemacs-core-modules)
  (message "[MinEmacs:Warn] The `me-completion', `me-keybindings' and `me-evil' modules have been moved to `minemacs-modules'. The `minemacs-core-modules' variable is now obsolete."))

;; MinEmacs 7.0.0 uses only `minemacs-modules'. The `minemacs-core-modules' is left for now just to ensure compatibility.
(setq minemacs-modules (cl-delete-if (+apply-partially-right #'memq '(me-splash me-bootstrap me-builtin me-compat me-gc))
                                     (delete-dups (append (bound-and-true-p minemacs-core-modules) minemacs-modules))))

;; Load modules
(mapc #'+load (mapcar (apply-partially #'format "%s%s.el" minemacs-core-dir) '(me-bootstrap me-compat me-builtin me-gc)))
(mapc #'+load (mapcar (apply-partially #'format "%s%s.el" minemacs-modules-dir) minemacs-modules))

;; Run hooks
(run-hooks 'minemacs-after-loading-modules-hook)

;; Write user custom variables to separate file instead of "init.el"
(setq custom-file (concat minemacs-config-dir "custom-vars.el"))

;; Load the custom variables file if it exists
(when (file-exists-p custom-file) (+load custom-file))

;; Load user configuration
(+load-user-configs 'config 'local/config)


(+log! "Loaded init.el")

;;; init.el ends here
