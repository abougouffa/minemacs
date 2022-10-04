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

(defconst LINUX-P (memq system-type '(gnu gnu/linux)))
(defconst BSD-P (memq system-type '(darwin berkeley-unix)))
(defconst WIN-P (memq system-type '(cygwin windwos-nt ms-dos)))
(defconst MAC-P (eq system-type 'darwin))

(defvar minemacs-after-startup nil
  "This hook will be run after loading Emacs.")

(defvar minemacs-first-file nil
  "This hook will be fired when the first file is open.")

;; This will be set by the virtual package `minemacs-loaded'
(defvar minemacs-loaded-p nil
  "MinEmacs has been loaded.")

(provide 'me-vars)
