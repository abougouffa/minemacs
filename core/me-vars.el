;; -*- lexical-binding: t; -*-

;;; MinEmacs directories

(defvar minemacs-config-dir
  (or (getenv "MINEMACS_DIR")
      ;; Temporary use this
      (when (file-directory-p user-emacs-directory)
        user-emacs-directory)
      (expand-file-name "~/.minemacs.d/")))

(unless (file-exists-p minemacs-config-dir)
  (mkdir minemacs-config-dir t))

(defvar minemacs-etc-dir (expand-file-name "etc/" user-emacs-directory))
(defvar minemacs-var-dir (expand-file-name "var/" user-emacs-directory))
(defvar minemacs-cache-dir (expand-file-name "cache/" minemacs-var-dir))
(defvar minemacs-autoloads-dir (expand-file-name "core/autoloads" user-emacs-directory))
(defvar minemacs-autoloads-file (expand-file-name "core/me-autoloads.el" user-emacs-directory))

(defconst sys/linux (memq system-type '(gnu gnu/linux)))
(defconst sys/bsd (memq system-type '(darwin berkeley-unix)))
(defconst sys/win (memq system-type '(cygwin windwos-nt ms-dos)))
(defconst sys/mac (eq system-type 'darwin))

(let ((feats (split-string system-configuration-features)))
  (defconst feat/gpm (member "GPM" feats))
  (defconst feat/cairo (member "CAIRO" feats))
  (defconst feat/lucid (member "LUCID" feats))
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

(defconst me-default-fonts
  '(:font-family "DejaVu Sans Mono"
                 :font-size 15
                 :variable-pitch-font-family "DejaVu Sans"
                 :variable-pitch-font-size 15))

(defvar me-fonts nil)


(provide 'me-vars)
