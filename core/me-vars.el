;; me-vars.el --- MinEmacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>

;;; MinEmacs directories

(defconst minemacs-config-dir
  (or (getenv "MINEMACS_DIR")
      (expand-file-name "~/.minemacs.d/")))

(defconst minemacs-verbose
  (not (null (or (getenv "MINEMACS_VERBOSE") init-file-debug))))

(defconst minemacs-root-dir (expand-file-name user-emacs-directory))
(defconst minemacs-core-dir (expand-file-name "core/" minemacs-root-dir))
(defconst minemacs-modules-dir (expand-file-name "modules/" minemacs-root-dir))

(defconst minemacs-local-dir (expand-file-name "local/" minemacs-root-dir))
(defconst minemacs-cache-dir (expand-file-name "cache/" minemacs-local-dir))
(defconst minemacs-autoloads-dirs (list (expand-file-name "autoloads" minemacs-core-dir)
                                        (expand-file-name "autoloads" minemacs-modules-dir)))
(defconst minemacs-autoloads-file (expand-file-name "me-autoloads.el" minemacs-core-dir))

;; Replace the default Emacs directory with /var
(setq user-emacs-directory minemacs-local-dir)

(defconst os/linux (not (null (memq system-type '(gnu gnu/linux)))))
(defconst os/bsd (not (null (memq system-type '(darwin berkeley-unix)))))
(defconst os/win (not (null (memq system-type '(cygwin windwos-nt ms-dos)))))
(defconst os/mac (eq system-type 'darwin))

(let ((feats (split-string system-configuration-features)))
  (defconst feat/gpm (not (null (member "GPM" feats))))
  (defconst feat/cairo (not (null (member "CAIRO" feats))))
  (defconst feat/lucid (not (null (member "LUCID" feats))))
  (defconst feat/modules (not (null (member "MODULES" feats))))
  (defconst feat/harfbuzz (not (null (member "HARFBUZZ" feats))))
  (defconst feat/xwidgets (not (null (member "XWIDGETS" feats))))
  (defconst feat/nativecomp (not (null (member "NATIVE_COMP" feats)))))

(defcustom minemacs-after-startup-hook nil
  "This hook will be run after loading Emacs."
  :group 'minemacs
  :type 'hook
  :local 'permenant-local)

;; This will be set by the virtual package `minemacs-loaded'
(defvar minemacs-loaded nil
  "MinEmacs has been loaded.")

(defvar minemacs-loaded-stage-1 nil
  "MinEmacs has been loaded.")

(defvar minemacs-loaded-stage-2 nil
  "MinEmacs has been loaded.")

(defconst me-default-fonts
  '(:font-family "DejaVu Sans Mono"
    :font-size 15
    :variable-pitch-font-family "DejaVu Sans"
    :variable-pitch-font-size 15))

(defvar me-fonts nil)

(defvar me-deps-executables
  '(grep find tar zip unzip zstd bzip2 gzip file ssh
    fd rg curl mu msmtp mbsync git mpv valgrind
    python gcc gdb cmake make clang-format rosbag
    cargo clangd ccls maxima octave fortune)
  "A list of programs I use within Emacs.")


(provide 'me-vars)
