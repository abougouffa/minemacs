;; me-vars.el --- MinEmacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>

;;; MinEmacs directories

(defconst minemacs-config-dir
  (or (getenv "MINEMACS_DIR")
      (expand-file-name "~/.minemacs.d/")))

(defconst minemacs-verbose (not (null (getenv "MINEMACS_VERBOSE"))))

(unless (file-exists-p minemacs-config-dir)
  (mkdir minemacs-config-dir t))

(defconst minemacs-etc-dir (expand-file-name "etc/" user-emacs-directory))
(defconst minemacs-var-dir (expand-file-name "var/" user-emacs-directory))
(defconst minemacs-cache-dir (expand-file-name "cache/" minemacs-var-dir))
(defconst minemacs-autoloads-dirs (list (expand-file-name "core/autoloads" user-emacs-directory)
                                        (expand-file-name "modules/autoloads" user-emacs-directory)))
(defconst minemacs-autoloads-file (expand-file-name "core/me-autoloads.el" user-emacs-directory))

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
