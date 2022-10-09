;; -*- lexical-binding: t; -*-

;;; MinEmacs directories

(defvar minemacs-config-dir
  (or (getenv "MINEMACS_DIR")
      (expand-file-name "~/.minemacs.d/")))

(defvar minemacs-verbose (not (null (getenv "MINEMACS_VERBOSE"))))

(unless (file-exists-p minemacs-config-dir)
  (mkdir minemacs-config-dir t))

(defvar minemacs-etc-dir (expand-file-name "etc/" user-emacs-directory))
(defvar minemacs-var-dir (expand-file-name "var/" user-emacs-directory))
(defvar minemacs-cache-dir (expand-file-name "cache/" minemacs-var-dir))
(defvar minemacs-autoloads-dirs (list (expand-file-name "core/autoloads" user-emacs-directory)
                                      (expand-file-name "modules/autoloads" user-emacs-directory)))
(defvar minemacs-autoloads-file (expand-file-name "core/me-autoloads.el" user-emacs-directory))

(defconst sys/linux (memq system-type '(gnu gnu/linux)))
(defconst sys/bsd (memq system-type '(darwin berkeley-unix)))
(defconst sys/win (memq system-type '(cygwin windwos-nt ms-dos)))
(defconst sys/mac (eq system-type 'darwin))

(let ((feats (split-string system-configuration-features)))
  (defconst feat/gpm (member "GPM" feats))
  (defconst feat/cairo (member "CAIRO" feats))
  (defconst feat/lucid (member "LUCID" feats))
  (defconst feat/modules (member "MODULES" feats))
  (defconst feat/harfbuzz (member "HARFBUZZ" feats))
  (defconst feat/xwidgets (member "XWIDGETS" feats))
  (defconst feat/nativecomp (member "NATIVE_COMP" feats)))

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
    python gcc gdb cmake make clang-format
    cargo clangd ccls maxima octave fortune)
  "A list of programs I use within Emacs.")


(provide 'me-vars)
