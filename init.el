;; init.el --- MinEmacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>

;; Check if Emacs version is supported
(let ((supported-version "28.2")
      (minimum-version "28.0.50"))
  (when (version< emacs-version supported-version)
    (if (version<= minimum-version emacs-version)
        (message
         "Warning: MinEmacs requires v%s or higher. \
You are running Emacs v%s, this version should work BUT IT IS NOT TESTED."
         supported-version emacs-version)
      (error "Emacs v%s is not supported, MinEmacs requires v%s or higher"
             emacs-version supported-version))))

;; HACK: At this point, MinEmacs variables defined in `me-vars' should be
;; already loaded (in "early-init.el"). However, we double-check here and load
;; them if necessary in case Emacs has been loaded directly from "init.el"
;; without passing by "early-init.el". This can happen when we are running in a
;; `me-org-export-async-init' context, or if we use some bootstrapping mechanism
;; like Chemacs2.
(unless (featurep 'me-vars)
  ;; Load MinEmacs variables from `me-vars'
  (load (expand-file-name "core/me-vars.el" (file-name-directory (file-truename load-file-name))) nil t))

;; HACK: Most core and third-party packages depends on the
;; `user-emacs-directory' variable to store some cache information and generated
;; configuration files. However, this will mess with MinEmacs' directory (which
;; defaults to `user-emacs-directory'). To keep the "~/.emacs.d/" directory
;; clean, we overwrite the `user-emacs-directory' with `minemacs-local-dir' so
;; all generated files gets stored in "~/.emacs.d/local/".
;; BUG: It is important to set this here and not in `me-vars' nor in
;; "early-init.el", otherwise, it won't work with Chemacs2-based installations.
(setq user-emacs-directory minemacs-local-dir)

;; HACK: Load Emacs 29 back ports for earlier Emacs versions. Note that I do
;; only back port a very small number of the functions/variables that I use from
;; Emacs29+ to be compatible with Emacs 28.2. If you need a complete forward
;; compatibility, you can install and use the `compat' library.
(when (< emacs-major-version 29)
  (load (concat minemacs-modules-dir "me-backports-29.el") nil (not minemacs-verbose)))

(setq
 ;; Enable debugging on error when Emacs is launched with the "--debug-init"
 ;; option or when the environment variable "$MINEMACS_DEBUG" is defined (see
 ;; `me-vars').
 debug-on-error minemacs-debug
 ;; Make byte compilation less noisy
 byte-compile-warnings minemacs-verbose
 byte-compile-verbose minemacs-verbose)

;; Native compilation settings
(when (featurep 'native-compile)
  (setq
   ;; Silence compiler warnings as they can be pretty disruptive, unless we are
   ;; running in `minemacs-verbose' mode.
   native-comp-async-report-warnings-errors (when minemacs-verbose 'silent)
   native-comp-verbose (if minemacs-verbose 3 0))

  ;; Make native compilation happens asynchronously.
  ;; HACK: Temporary support the `native-comp-deferred-compilation' variable
  ;; deprecated late in Emacs29.
  (set (if (boundp 'native-comp-deferred-compilation)
           'native-comp-deferred-compilation ; Deprecated in Emacs29
         'native-comp-jit-compilation)
       t)

  ;; Set the right directory to store the native compilation cache to avoid
  ;; messing with "~/.emacs.d/".
  (startup-redirect-eln-cache (concat minemacs-cache-dir "eln/")))

;; Add some of MinEmacs' directories to `load-path'.
(add-to-list 'load-path minemacs-core-dir)
(add-to-list 'load-path minemacs-elisp-dir)
(add-to-list 'load-path minemacs-extras-dir)

(defun minemacs-generate-autoloads ()
  "Generate MinEmacs' autoloads file."
  (interactive)
  (when (file-exists-p minemacs-autoloads-file)
    (delete-file minemacs-autoloads-file))

  (let ((autoload-dirs nil))
    (dolist (dir (list minemacs-core-dir minemacs-extras-dir minemacs-elisp-dir))
      (when (file-directory-p dir)
        (setq autoload-dirs
              (append autoload-dirs
                      (list dir)
                      (seq-filter
                       #'file-directory-p
                       (directory-files-recursively dir ".*" t))))))
    (loaddefs-generate autoload-dirs minemacs-autoloads-file)))

;; Generate auto-loads if they don't exist
(unless (file-exists-p minemacs-autoloads-file)
  (minemacs-generate-autoloads))

;; Load autoloads file
(load minemacs-autoloads-file nil (not minemacs-verbose))

;; Load environment variables when available.
;; HACK: When Emacs is launched from the terminal (in GNU/Linux), it inherits
;; the terminal's environment variables, which can be useful specially for
;; running commands under a custom "$PATH" directory. But when Emacs is launched
;; from the desktop manager (KDE, Gnome, etc.), it can omit the terminal's
;; environment variables. The way I solve this is by launching Emacs from
;; terminal, which gives Emacs the full environment variables of the invoking
;; terminal. Then I call the `+env-save' command, which reads the environment
;; variables defined in `+env-save-vars' and stores them in
;; "~/.emacs.d/local/system-env.el". This file is then loaded in the future
;; Emacs sessions (launched either from terminal or from GUI) by calling the
;; `+env-load' command.
(+env-load)

;; NOTE: This is MinEmacs' synchronization point. To get a fast Emacs startup,
;; MinEmacs tries to defer loading packages until this hook is executed. This is
;; managed by the `minemacs-loaded' and `minemacs-lazy' pseudo-packages. After
;; loading Emacs, the `emacs-startup-hook' gets executed, we use this hook to
;; profile the startup time, load the fonts and the theme, and setup the
;; *scratch* buffer content. Lastly we trigger the `minemacs-loaded'
;; synchronization module, which runs the `minemacs-after-startup-hook' hooks
;; and provide `minemacs-loaded' so the packages loaded with `:after
;; minemacs-loaded' can be loaded. The `minemacs-loaded' will trigger
;; `minemacs-lazy', which provides `minemacs-lazy' so the packages loaded with
;; `:after minemacs-lazy' can be loaded and incrementally run the hooks in
;; `minemacs-lazy-hook' when Emacs gets idle.
(add-hook
 'emacs-startup-hook
 (defun +minemacs--loaded-h ()
   (+log! "=============== Loaded Emacs ===============")
   (+info! "Loaded Emacs in %s." (emacs-init-time))

   ;; When running in an async Org export context, there is no need to set
   ;; the fonts, load the theme or play with the scratch buffer.
   (unless (featurep 'me-org-export-async-init)
     (+log! "Applying `minemacs-fonts'.")
     ;; Load fonts, values are read from `minemacs-fonts' if set in config.el,
     ;; otherwise, they are read from the default `minemacs-default-fonts'.
     (+set-fonts)

     ;; Initially MinEmacs loads the `doom-one-light' theme, and when
     ;; `minemacs-theme' is set in user configuration, it is loaded here.
     (+load-theme)

     (+log! "Setting scratch buffer content.")
     ;; Print load time, and a quote to *scratch*
     (with-current-buffer (get-scratch-buffer-create)
       (erase-buffer)
       (insert (format
                ";; MinEmacs loaded in %.2fs with %d garbage collection%s done!\n"
                (string-to-number (car (string-split (emacs-init-time))))
                gcs-done (if (> gcs-done 1) "s" "")))
       (insert ";; ==============================\n")
       ;; Insert some quote from fortune
       (when (executable-find "fortune")
         (insert (string-join
                  (mapcar (lambda (l) (concat ";; " l))
                          (string-lines (shell-command-to-string "fortune")))
                  "\n"))
         (insert "\n;; ==============================\n"))
       ;; Set initial scratch message.
       (setq initial-scratch-message (buffer-string)))

     ;; In `me-defaults', the `initial-major-mode' is set to `fundamental-mode'
     ;; to enhance startup time. However, I like to use the scratch buffer to
     ;; evaluate Elisp code, so we switch to Elisp mode in the scratch buffer
     ;; when Emacs is idle for 10 seconds.
     (+eval-when-idle-for! 10.0
       (setq initial-major-mode 'emacs-lisp-mode)
       (with-current-buffer (get-scratch-buffer-create)
         (emacs-lisp-mode))))

   ;; Require the virtual package to triggre loading packages depending on it
   (require 'minemacs-loaded)))

;; ========= Load MinEmacs packages and user customization =========
;; When running in an async Org export context, the used modules are set in
;; modules/extras/me-org-export-async-init.el, so we must not overrite them with
;; the user's enabled modules.
(if (featurep 'me-org-export-async-init)
    (progn (message "Loading \"init.el\" in an org-export-async context.")
           (setq minemacs-not-lazy t)
           (require 'minemacs-loaded))
  ;; Load the default list of enabled modules (`minemacs-modules' and `minemacs-core-modules')
  (load (concat minemacs-core-dir "me-modules.el") nil (not minemacs-verbose))

  ;; The modules.el file can override minemacs-modules and minemacs-core-modules
  (let ((user-conf-modules (concat minemacs-config-dir "modules.el")))
    (when (file-exists-p user-conf-modules)
      (+log! "Loading modules file from \"%s\"" user-conf-modules)
      (load user-conf-modules nil (not minemacs-verbose)))))

;; Load fonts early (they are read from the default `minemacs-default-fonts').
(+set-fonts)

;; NOTE: Ensure the `me-gc' module is in the core modules list. This module
;; enables the `gcmh-mode' package (a.k.a. the Garbage Collector Magic Hack).
;; This GCMH minimizes GC interference with the activity by using a high GC
;; threshold during normal use, then when Emacs is idling, GC is triggered and a
;; low threshold is set. In MinEmacs, we set the threshold (`gc-cons-threshold'
;; variable) to an unlimited size in "early-init.el", this helps improving the
;; startup time, but needs to be set down to a more reasonable value after Emacs
;; gets loaded. The use of `gcmh-mode' ensures reverting this value so we don't
;; need to do it manually.
(add-to-list 'minemacs-core-modules 'me-gc t)

;; Load MinEmacs core modules
(dolist (module minemacs-core-modules)
  (+log! "Loading core module \"%s\"" module)
  (let ((filename (concat minemacs-core-dir (format "%s.el" module))))
    (if (file-exists-p filename)
        (load filename nil (not minemacs-verbose))
      (+error! "Core module \"%s\" not found!" module))))

;; Load MinEmacs modules
(dolist (module minemacs-modules)
  (+log! "Loading module \"%s\"" module)
  (let ((filename (concat minemacs-modules-dir (format "%s.el" module))))
    (if (file-exists-p filename)
        (load filename nil (not minemacs-verbose))
      (+error! "Module \"%s\" not found!" module))))

;; Run the hooks in `minemacs-before-user-config-hook' before loading the user
;; configuration and custom variables
(require 'minemacs-before-user-config)

;; Write user custom variables to separate file instead of "init.el"
(setq custom-file (concat minemacs-config-dir "custom-vars.el"))

;; Load the custom variables file if it exists
(when (and custom-file (file-exists-p custom-file))
  (+log! "Loafing user custom file from \"%s\"" custom-file)
  (load custom-file nil (not minemacs-verbose)))

;; Load user configuration from "$MINEMACSDIR/config.el" when available
(let ((user-config (concat minemacs-config-dir "config.el")))
  (when (file-exists-p user-config)
    (+log! "Loading user config file from \"%s\"" user-config)
    (load user-config nil (not minemacs-verbose))))

(with-eval-after-load 'minemacs-loaded
  ;; Delete outdated natively compiled files when Emacs become idle
  (when (featurep 'native-compile)
    (+eval-when-idle!
      (+info! "Trying to clean outdated native compile cache")
      (+shutup! (native-compile-prune-cache)))))

(+log! "Loaded init.el")
