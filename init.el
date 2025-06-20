;; init.el --- MinEmacs core initialization file -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2022-09-17
;; Last modified: 2025-06-15

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

(unless (featurep 'early-init) ; In case we decided to do some funny loading without the `early-init-file'
  (load (expand-file-name "early-init.el" (file-name-directory (file-truename load-file-name)))))

(require 'me-lib) ; Load MinEmacs' core library
(require 'use-package)

(setq custom-file (concat minemacs-config-dir "custom-vars.el"))
(when (file-exists-p custom-file) (+load custom-file))

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

;; This is MinEmacs' synchronization point, add it to the very beginning of `emacs-startup-hook'
(add-hook
 'emacs-startup-hook
 (lambda ()
   (+info! "Emacs%s loaded in %.3fs, including %.3fs for %d GCs." (if (daemonp) " (in daemon mode)" "")
           (float-time (time-subtract after-init-time before-init-time)) gc-elapsed gcs-done)
   (+load-theme)
   (require 'minemacs-loaded))
 -91)

;; ========= Make some special hooks =========
(+make-first-file-hook! 'org "\\.org$")
(+make-first-file-hook! 'elisp "\\.elc?$")
(+make-first-file-hook! 'python (rx "." (or "py" "pyw" "pyx" "pyz" "pyzw") eol))
(+make-first-file-hook! 'c/c++ (rx "." (or "c" "cpp" "cxx" "cc" "c++" "h" "hpp" "hxx" "hh" "h++" "ixx" "cppm" "cxxm" "c++m" "ccm") eol))
(+make-first-file-hook! nil ".")

;; ========= Load MinEmacs packages and user customization =========
;; Load the default list of enabled modules `minemacs-modules'
(+load-user-configs 'modules 'local/modules)

;; When the MINEMACS_LOAD_ALL_MODULES environment variable is set, we force
;; loading all modules, including on-demand ones.
(when minemacs-load-all-modules-p (setq minemacs-modules (minemacs-modules t)))

;; When only built-in packages are loaded, we define `:straight' to do nothing.
;; This is important for the updates installed for built-in packages in
;; `me-builtin' like `transient', `eglot', `xref', etc.
(if minemacs-builtin-only-p
    (progn
      (add-to-list 'use-package-keywords :straight)
      (defalias 'use-package-normalize/:straight #'ignore)
      (defun use-package-handler/:straight (name _keyword _arg rest state)
        (use-package-concat (use-package-process-keywords name rest state))))
  (require 'me-bootstrap))

(require 'once)
(require 'satch)
(require 'me-builtin)

(unless minemacs-builtin-only-p
  (mapc #'+load (mapcar (apply-partially #'format "%s%s.el" minemacs-modules-dir) minemacs-modules)))

(unless +use-package-keep-checking-for-disabled-p
  (advice-remove 'use-package '+use-package--check-if-disabled:around-a))

(+load-user-configs 'config 'local/config) ; Load user configuration

(+log! "Loaded init.el")

;;; init.el ends here
