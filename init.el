;; init.el --- MinEmacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>

;; Check if Emacs version is supported
(let ((min-version "28.2"))
  (when (version< emacs-version min-version)
    (error "Emacs v%s is not supported, MinEmacs requires v%s or higher"
           emacs-version min-version)))

;; Don't attempt to find/apply special file handlers to files loaded during
;; startup, this should seedup the startup process.
(let (file-name-handler-alist)
  ;; Load vars if they have not been loaded (if Emacs has been loaded directly
  ;; from "init.el" without passing by "early-init.el")
  (unless (featurep 'me-vars)
    ;; Load MinEmacs variables first
    (load (expand-file-name "core/me-vars.el" (file-name-directory (file-truename load-file-name))) nil t))

  ;; Overwrite `user-emacs-directory' with `minemacs-local-dir'
  (setq user-emacs-directory minemacs-local-dir)

  ;; Load Emacs 29 backports for earlier Emacs versions
  (when (< emacs-major-version 29)
    (load (concat minemacs-modules-dir "me-backports-29.el") nil (not minemacs-verbose)))

  (setq
   ;; Enable debugging on error when env variable "MINEMACS_DEBUG" is defined
   debug-on-error minemacs-debug
   ;; Make byte compilation less verbose
   byte-compile-warnings minemacs-verbose
   byte-compile-verbose minemacs-verbose)

  ;; Native compilation settings
  (when (featurep 'native-compile)
    (setq
     ;; Silence compiler warnings as they can be pretty disruptive
     native-comp-async-report-warnings-errors (when minemacs-verbose 'silent)
     native-comp-verbose (if minemacs-verbose 3 0)
     ;; Make native compilation happens asynchronously
     inhibit-automatic-native-compilation nil)

    ;; Set the right directory to store the native compilation cache
    ;; NOTE: The `startup-redirect-eln-cache' function is available in Emacs 29,
    ;; it is back ported in MinEmacs for Emacs 28.
    (startup-redirect-eln-cache (concat minemacs-cache-dir "eln/")))

  ;; Add direcotries to `load-path'
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

  ;; Load environment variables when available
  (+env-load)

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
         (emacs-lisp-mode)))

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

  ;; Ensure me-gc is in the list
  (add-to-list 'minemacs-core-modules 'me-gc t)

  ;; Core modules
  (dolist (module minemacs-core-modules)
    (+log! "Loading core module \"%s\"" module)
    (let ((filename (concat minemacs-core-dir (format "%s.el" module))))
      (if (file-exists-p filename)
          (load filename nil (not minemacs-verbose))
        (+error! "Core module \"%s\" not found!" module))))

  ;; Modules
  (dolist (module minemacs-modules)
    (+log! "Loading module \"%s\"" module)
    (let ((filename (concat minemacs-modules-dir (format "%s.el" module))))
      (if (file-exists-p filename)
          (load filename nil (not minemacs-verbose))
        (+error! "Module \"%s\" not found!" module))))

  ;; Run hooks before loading user config
  (require 'minemacs-before-user-config)

  ;; Write user custom variables to separate file instead of init.el
  (setq custom-file (concat minemacs-config-dir "custom-vars.el"))

  (when (and custom-file (file-exists-p custom-file))
    (+log! "Loafing user custom file from \"%s\"" custom-file)
    (load custom-file nil (not minemacs-verbose)))

  ;; Load user config when available
  (let ((user-config (concat minemacs-config-dir "config.el")))
    (when (file-exists-p user-config)
      (+log! "Loading user config file from \"%s\"" user-config)
      (load user-config nil (not minemacs-verbose))))

  (with-eval-after-load 'minemacs-loaded
    ;; Delete outdated natively compiled files
    (when (featurep 'native-compile)
      (+eval-when-idle!
        (+info! "Trying to clean outdated native compile cache")
        (+shutup! (native-compile-prune-cache)))))

  (+log! "Loaded init.el"))
