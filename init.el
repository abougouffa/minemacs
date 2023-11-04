;; init.el --- MinEmacs core initialization file -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;            ..                        ...                                   ..:::::.....       .::::.     ..
;;         .:::.                       :::::                                  :::::::::::::::::::::::::     .:::.
;;        :::                          .:::.                                  .::::::.        ...:::::.        :::
;;       ::.                                                                   .:::::                           .::
;;      ::.       ::::::::..:::    :::::::       .::::.:::::.                    .::::.                          .::
;;     .::        .::: .::: :::.   ...::::        :::::..::::                       .:::.                         ::.
;;     ::.        .::. :::. :::       :::.        :::.    :::.               ..........::::.                      .::
;;     ::.        :::  :::  :::       :::        .:::     :::          ..::::::::::::::::::::.                    .::
;;     ::.       .::: .::: .::.      .:::        ::::    .:::        .::::::::::...                               .::
;;     :::       :::. :::. :::       :::.   ..   :::.    :::.        :::::::::                                    :::
;;      ::       :::  :::  :::.      ::::.::::  .:::    .::::         ::::::::                                    ::
;;      .::      ::.  ::. .::::       .:::::.   .::.    .::::          .::::::::.                                ::.
;;       :::                                                              .::::::::...             :.           :::
;;        .::.                                                               ..::::::::::::.......:::.        .::.
;;          .::.                                                                   ..:::::::::::::::.       .::.
;;
;;                                 MINIMALIST & LIGHTWEIGHT EMACS CONFIGURATION FRAMEWORK
;;                                                          abougouffa.github.io/minemacs

;; Load and hooks order:
;; - ~/.emacs.d/early-init.el
;; - $MINEMACSDIR/early-config.el (unless $MINEMACS_IGNORE_USER_CONFIG or $MINEMACS_IGNORE_EARLY_CONFIG_EL)
;; - ~/.emacs.d/init.el
;;   + `before-init-hook'
;;   + ~/.emacs.d/core/me-vars.el
;;   + ~/.emacs.d/core/backports/*.el (when Emacs < 29)
;;   + ~/.emacs.d/core/me-loaddefs.el
;;   + ~/.emacs.d/core/init-tweaks.el (unless $MINEMACS_IGNORE_USER_CONFIG or $MINEMACS_IGNORE_INIT_TWEAKS_EL)
;;   + $MINEMACSDIR/modules.el (unless $MINEMACS_IGNORE_USER_CONFIG or $MINEMACS_IGNORE_MODULES_EL)
;;   + ~/.emacs.d/core/[minemacs-core-modules].el
;;   + ~/.emacs.d/modules/[minemacs-modules].el
;;   + `minemacs-after-loading-modules-hook'
;;   + $MINEMACSDIR/custom-vars.el
;;   + $MINEMACSDIR/config.el (unless $MINEMACS_IGNORE_USER_CONFIG or $MINEMACS_IGNORE_CONFIG_EL)
;;   + `after-init-hook'
;;   + `emacs-startup-hook'
;;   + `minemacs-after-startup-hook'
;;     - `minemacs-lazy-hook' (delayed)

;; Special hooks defined with `+make-first-file-hook!'
;; - `minemacs-first-file-hook'
;; - `minemacs-first-elisp-file-hook'
;; - `minemacs-first-org-file-hook'

;;; Code:

;; Check if Emacs version is supported. You can define the
;; $MINEMACS_IGNORE_VERSION_CHECK environment variable to ignore this check.
;; This can be useful if you are stuck with an old Emacs version and you've
;; incrementally implemented the new Emacs routines MinEmacs needs in your
;; "init-tweaks.el".
(let ((min-ver "28.0"))
  (when (and (version< emacs-version min-ver) (not (getenv "MINEMACS_IGNORE_VERSION_CHECK")))
    (error "Emacs v%s is not supported, MinEmacs requires v%s or higher" emacs-version min-ver)))

;; PERF: Setting `file-name-handler-alist' to nil should boost startup time.
;; reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start
;; Store the current value so we can reset it after Emacs startup.
(put 'file-name-handler-alist 'original-value (default-toplevel-value 'file-name-handler-alist))
;; Make sure the new value survives any current let-binding.
(set-default-toplevel-value 'file-name-handler-alist nil)
;; After Emacs startup, we restore `file-name-handler-alist' while conserving
;; the potential edits made during startup.
(add-hook
 'emacs-startup-hook
 (defun +mineamcs--restore-file-name-handler-alist-h ()
   (setq file-name-handler-alist
         (delete-dups
          (append file-name-handler-alist
                  (get 'file-name-handler-alist 'original-value)))))
 101)

;; HACK: At this point, MinEmacs variables defined in `me-vars' should be
;; already loaded (in "early-init.el"). However, we double-check here and load
;; them if necessary in case Emacs has been loaded directly from "init.el"
;; without passing by "early-init.el". This can happen when we are running in a
;; `me-org-export-async-init' context, or if we use some bootstrapping mechanism
;; like Chemacs2.
(unless (featurep 'me-vars)
  (load (expand-file-name "core/me-vars.el" (file-name-directory (file-truename load-file-name))) nil t))

(defun +load (&rest filename-parts)
  "Load a file, the FILENAME-PARTS are concatenated to form the file name."
  (let ((filename (file-truename (apply #'concat filename-parts))))
    (if (file-exists-p filename)
        (load filename nil (not minemacs-verbose))
      (message "[MinEmacs:Error] Cannot load \"%s\", the file doesn't exists." filename))))

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
;; only back port a very small number of the functions/variables that I use at
;; early stage from Emacs29+ to be compatible with Emacs 28.2. For any Emacs
;; version less than 29, MinEmacs will enable the `me-compat' module and load it
;; just after `me-bootstrap'. This module loads the `compat' package which
;; provide several forward compatibility functions, it is loaded at an early
;; stage to provide its functionality to the rest of the modules so we can use
;; some new features when configuring them.
(when (< emacs-major-version 29)
  (let ((backports-dir (concat minemacs-core-dir "backports/")))
    (mapc (apply-partially #'+load backports-dir)
          (directory-files backports-dir nil "\\.el$"))))

(setq
 ;; Enable debugging on error when Emacs is launched with the "--debug-init"
 ;; option or when the environment variable "$MINEMACS_DEBUG" is defined (see
 ;; `me-vars').
 debug-on-error minemacs-debug
 ;; Decrese the warning type to `:error', unless we are running in verbose mode
 warning-minimum-level (if minemacs-verbose :warning :error)
 warning-minimum-log-level warning-minimum-level
 ;; Make byte compilation less noisy
 byte-compile-warnings minemacs-verbose
 byte-compile-verbose minemacs-verbose)

;; Native compilation settings
(when (featurep 'native-compile)
  (setq
   ;; Silence compiler warnings as they can be pretty disruptive, unless we are
   ;; running in `minemacs-verbose' mode.
   native-comp-async-report-warnings-errors (when minemacs-verbose 'silent)
   native-comp-verbose (if minemacs-verbose 1 0) ; do not be too verbose
   native-comp-debug (if minemacs-debug 1 0)
   ;; Make native compilation happens asynchronously.
   native-comp-jit-compilation t)

  ;; Set the right directory to store the native compilation cache to avoid
  ;; messing with "~/.emacs.d/".
  (startup-redirect-eln-cache (concat minemacs-cache-dir "eln/")))

;; Add some of MinEmacs' directories to `load-path'.
(setq load-path (append (list minemacs-core-dir minemacs-elisp-dir minemacs-extras-dir) load-path))

(defun minemacs-generate-loaddefs ()
  "Generate MinEmacs' loaddefs file."
  (interactive)
  (when (file-exists-p minemacs-loaddefs-file)
    (delete-file minemacs-loaddefs-file))

  (loaddefs-generate
   (list minemacs-core-dir minemacs-elisp-dir minemacs-extras-dir)
   minemacs-loaddefs-file))

;; Some of MinEmacs commands and libraries are defined to be auto-loaded. In
;; particular, these in the `minemacs-core-dir', `minemacs-elisp-dir', and
;; `minemacs-extras-dir' directories. The generated loaddefs file will be stored
;; in `minemacs-loaddefs-file'. We first regenerate the loaddefs file if it
;; doesn't exist.
(unless (file-exists-p minemacs-loaddefs-file)
  (minemacs-generate-loaddefs))

;; Then we load the loaddefs file
(+load minemacs-loaddefs-file)

;; Load user init tweaks from "$MINEMACSDIR/init-tweaks.el" when available
(unless (memq 'init-tweaks minemacs-ignore-user-config)
  (let ((user-init-tweaks (concat minemacs-config-dir "init-tweaks.el")))
    (when (file-exists-p user-init-tweaks)
      (+load user-init-tweaks))))

;; HACK: Load the environment variables saved from shell using `+env-save' to
;; `+env-file'. `+env-save' saves all environment variables except these matched
;; by `+env-deny-vars'.
(+env-load) ; Load environment variables when available.

;; NOTE: This is MinEmacs' synchronization point. To get a fast Emacs startup,
;; MinEmacs tries to defer loading most of its packages until this hook is
;; executed. This is managed by the `minemacs-loaded' and `minemacs-lazy'
;; pseudo-modules. After loading Emacs, the `emacs-startup-hook' gets executed,
;; we use this hook to profile the startup time, load the fonts and the theme,
;; and setup the *scratch* buffer content. Lastly we require the
;; `minemacs-loaded' synchronization module, which runs internally the
;; `minemacs-after-startup-hook' hooks and provide `minemacs-loaded' so the
;; packages loaded with `:after minemacs-loaded' can be loaded. The
;; `minemacs-loaded' will require `minemacs-lazy' when Emacs goes idle, this
;; pseudo-module provides `minemacs-lazy' so the packages loaded with `:after
;; minemacs-lazy' can be loaded then it incrementally run the hooks in
;; `minemacs-lazy-hook' when Emacs goes idle.
(defun +minemacs--loaded-h ()
  (+log! "=============== Loaded Emacs ===============")
  (+info! "Loaded Emacs in %s." (emacs-init-time))

  ;; When running in an async Org export context, there is no need to set
  ;; the fonts, load the theme or play with the scratch buffer.
  (unless (featurep 'me-org-export-async-init)
    (+log! "Applying `minemacs-fonts'.")
    ;; Initially MinEmacs loads the `doom-one-light' theme, and when
    ;; `minemacs-theme' is set in user configuration, it is loaded here.
    (+load-theme)

    (+deferred!
     (+log! "Loading the default persistent scratch buffer.")
     (let ((buf (current-buffer)))
       ;; Load the default persistent scratch buffer
       (+scratch-open-buffer nil nil 'same-window)
       ;; Switch to the previous buffer
       (switch-to-buffer buf)
       ;; And kill the Emacs' default scratch buffer
       (when-let ((s (get-buffer "*scratch*"))) (kill-buffer s))))

    ;; In `me-defaults', the `initial-major-mode' is set to `fundamental-mode'
    ;; to enhance startup time. However, I like to use the scratch buffer to
    ;; evaluate Elisp code, so we switch to Elisp mode in the scratch buffer
    ;; when Emacs is idle for 10 seconds.
    (+eval-when-idle-for! 10.0
      (setq initial-major-mode 'lisp-interaction-mode)
      (when-let ((scratch-buffer (get-buffer "*scratch*")))
        (with-current-buffer scratch-buffer
          (emacs-lisp-mode)))))

  ;; Require the virtual package to triggre loading packages depending on it
  (require 'minemacs-loaded))

;; Add it to the very begining of `emacs-startup-hook'
(add-hook 'emacs-startup-hook #'+minemacs--loaded-h -101)

;; ========= Make some special hooks =========
(+make-first-file-hook! 'org "\\.org$")
(+make-first-file-hook! 'elisp "\\.elc?$")
(+make-first-file-hook! nil ".")

;; ========= Load MinEmacs packages and user customization =========
;; When running in an async Org export context, the used modules are set in
;; modules/extras/me-org-export-async-init.el, so we must not overrite them with
;; the user's enabled modules.
(if (featurep 'me-org-export-async-init)
    (progn (message "Loading \"init.el\" in an org-export-async context.")
           (setq minemacs-not-lazy t)
           (require 'minemacs-loaded))
  ;; Load the default list of enabled modules (`minemacs-modules' and `minemacs-core-modules')
  (+load minemacs-core-dir "me-modules.el")

  (unless (memq 'modules minemacs-ignore-user-config)
    ;; The modules.el file can override minemacs-modules and minemacs-core-modules
    (let ((user-conf-modules (concat minemacs-config-dir "modules.el")))
      (when (file-exists-p user-conf-modules)
        (+load user-conf-modules)))))

;; NOTE: Ensure the `me-gc' module is in the core modules list. This module
;; enables the `gcmh-mode' package (a.k.a. the Garbage Collector Magic Hack).
;; This GCMH minimizes GC interference with the activity by using a high GC
;; threshold during normal use, then when Emacs is idling, GC is triggered and a
;; low threshold is set. In MinEmacs, we set the threshold (`gc-cons-threshold'
;; variable) to an unlimited size in "early-init.el", this helps improving the
;; startup time, but needs to be set down to a more reasonable value after Emacs
;; gets loaded. The use of `gcmh-mode' ensures reverting this value so we don't
;; need to do it manually.
;; NOTE: Ensure the `me-defaults', `me-splash', `me-bootstrap' and `me-compat'
;; modules are in the right order. The `me-compat' should be loaded just after
;; `me-bootstrap' once `straight' and `use-package' are set up. This enables us
;; to use some of the new Emacs 29 functions even on earlier Emacs versions,
;; this can be useful when configuring the module's packages and adding new
;; functionality.
(setq minemacs-core-modules
      (delete-dups
       (append
        '(me-defaults)
        (when (memq 'me-splash minemacs-core-modules) '(me-splash))
        '(me-bootstrap)
        (when (< emacs-major-version 29) '(me-compat))
        '(me-builtin me-gc me-fonts)
        minemacs-core-modules)))

;; Load MinEmacs modules
(dolist (module-file (append
                      (mapcar (apply-partially #'format "%s%s.el" minemacs-core-dir) minemacs-core-modules)
                      (mapcar (apply-partially #'format "%s%s.el" minemacs-modules-dir) minemacs-modules)))
  (+load module-file))

(run-hooks 'minemacs-after-loading-modules-hook)

;; Write user custom variables to separate file instead of "init.el"
(setq custom-file (concat minemacs-config-dir "custom-vars.el"))

;; Load the custom variables file if it exists
(when (file-exists-p custom-file)
  (+load custom-file))

;; Load user configuration from "$MINEMACSDIR/config.el" when available
(unless (memq 'config minemacs-ignore-user-config)
  (let ((user-config (concat minemacs-config-dir "config.el")))
    (when (file-exists-p user-config)
      (+load user-config))))

(+lazy!
 (when (featurep 'native-compile)
   (+info! "Trying to clean outdated native compile cache")
   ;; Delete outdated natively compiled files when Emacs become idle
   (+shutup! (native-compile-prune-cache)))
 (+info! "Trying to clean outdated straight build cache")
 (+shutup! (+straight-prune-build-cache))
 (+info! "Trying to clean MinEmacs' root directory")
 (+shutup! (+minemacs-root-dir-cleanup)))


(+log! "Loaded init.el")

;;; init.el ends here
