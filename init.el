;; init.el --- MinEmacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>

;; Load MinEmacs variables first
(load (concat user-emacs-directory "core/me-vars.el") nil :no-message)

;; Enable debugging on error when env variable "MINEMACS_DEBUG" is defined
(when minemacs-debug
  (setq debug-on-error t
        eval-expression-debug-on-error t))

;; Load environment variables when available
(let ((env-file (concat minemacs-local-dir "env")))
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
         (convert-standard-filename (concat minemacs-cache-dir "eln")))
      (startup-redirect-eln-cache (convert-standard-filename (concat minemacs-cache-dir "eln"))))))

;; Add direcotries to `load-path'
(add-to-list 'load-path minemacs-core-dir)
(add-to-list 'load-path (concat minemacs-root-dir "elisp/"))
(add-to-list 'load-path (concat minemacs-modules-dir "extras/"))

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
(setq custom-file (concat minemacs-config-dir "custom-vars.el"))

;;; Load the early config file if it exists
(let ((early-config-path (concat minemacs-config-dir "early-config.el")))
  (when (file-exists-p early-config-path)
    (load early-config-path nil 'nomessage)))

(defvar minemacs-core-modules
  '(defaults splash bootstrap core-ui keybindings evil completion))

(defvar minemacs-modules
  '(ui editor vc prog lisp data org notes window email docs
    natural-langs files tools biblio daemon rss ros eaf math media binary))

(defun minemacs-generate-autoloads ()
  (interactive)
  (when (file-exists-p minemacs-autoloads-file)
    (delete-file minemacs-autoloads-file))

  (let ((autoload-dirs nil)) 
    (dolist (dir (list minemacs-core-dir
                       minemacs-modules-dir
                       (concat minemacs-root-dir "elisp/")))
      (when (file-directory-p dir)
        (setq autoload-dirs
              (append
               autoload-dirs
               (list dir)
               (seq-filter
                #'file-directory-p
                (directory-files-recursively dir ".*" t))))))
    (if (<= emacs-major-version 28)
        (make-directory-autoloads autoload-dirs
                                  minemacs-autoloads-file)
      (loaddefs-generate autoload-dirs
                         minemacs-autoloads-file))))

;; Auto-loads
(unless (file-exists-p minemacs-autoloads-file)
  (minemacs-generate-autoloads))

;; Load autoloads file
(load minemacs-autoloads-file nil (not minemacs-verbose))

;; The modules.el file can override minemacs-modules and minemacs-core-modules
(let ((mods (concat minemacs-config-dir "modules.el")))
  (when (file-exists-p mods)
    (+log! "Loading modules file from \"%s\"" mods)
    (load mods nil (not minemacs-verbose))))

(defun minemacs-load (&optional load-core-modules)
  "Reload all configuration, including user's config.el."

  ;; Load fonts early (they are read from the default `minemacs-default-fonts').
  (+set-fonts)

  ;; Core modules
  (when load-core-modules
    (dolist (module minemacs-core-modules)
      (+log! "Loading core module \"%s\"" module)
      (let ((filename (concat minemacs-core-dir (format "me-%s.el" module))))
        (if (file-exists-p filename)
            (load filename nil (not minemacs-verbose))
          (+info! "Core module \"%s\" not found!" module)))))

  ;; Modules
  (dolist (module minemacs-modules)
    (+log! "Loading module \"%s\"" module)
    (let ((filename (concat minemacs-modules-dir (format "me-%s.el" module))))
      (if (file-exists-p filename)
          (load filename nil (not minemacs-verbose))
        (+info! "Module \"%s\" not found!" module))))

  (when (and custom-file (file-exists-p custom-file))
    (+log! "Loafing user customs from custom.el")
    (load custom-file nil (not minemacs-verbose)))

  ;; Load user config when available
  (let ((user-config (concat minemacs-config-dir "config.el")))
    (when (file-exists-p user-config)
      (+log! "Loading user config file from \"%s\"" user-config)
      (load user-config nil (not minemacs-verbose))))

  ;; Load GC module lastly
  (run-at-time
   5 nil
   (lambda ()
     (load (concat minemacs-core-dir "me-gc.el")
           nil (not minemacs-verbose)))))

;; Load for the first time
(minemacs-load t)

(+log! "Loaded early-config.el")
