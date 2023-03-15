;; me-vars.el --- MinEmacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; MinEmacs directories

(defconst minemacs-config-dir
  (file-name-as-directory
   (or (getenv "MINEMACS_DIR")
       (getenv "MINEMACSDIR")
       "~/.minemacs.d/"))
  "MinEmacs user customization directory.")

(defconst minemacs-debug
  (and (or (getenv "MINEMACS_DEBUG") init-file-debug) t)
  "MinEmacs is started in debug mode.")

(defconst minemacs-verbose
  (and (or (getenv "MINEMACS_VERBOSE") minemacs-debug) t)
  "MinEmacs is started in verbose mode.")

(defconst minemacs-not-lazy
  (or (daemonp) (and (getenv "MINEMACS_NOT_LAZY") t))
  "Load lazy packages (minemacs-lazy-hook) immediately.")

(defcustom minemacs-msg-level
  (let ((level (string-to-number (or (getenv "MINEMACS_MSG_LEVEL") ""))))
    (cond (minemacs-verbose 4)
          ((> level 0) level)
          (t 1)))
  "Level of printed messages.
1 - `+error!'
2 - `+info!'
3 - `+log!'
4 - `+debug!'"
  :group 'minemacs
  :type 'natnum)

;; Derive the root directory from this file path
(defconst minemacs-root-dir
  (abbreviate-file-name
   (file-name-directory
    (directory-file-name
     (file-name-directory (file-truename load-file-name))))))
(defconst minemacs-core-dir (concat minemacs-root-dir "core/"))
(defconst minemacs-elisp-dir (concat minemacs-root-dir "elisp/"))
(defconst minemacs-modules-dir (concat minemacs-root-dir "modules/"))
(defconst minemacs-extras-dir (concat minemacs-modules-dir "extras/"))
(defconst minemacs-local-dir (concat minemacs-root-dir "local/"))
(defconst minemacs-cache-dir (concat minemacs-local-dir "cache/"))
(defconst minemacs-loaddefs-file (concat minemacs-core-dir "me-loaddefs.el"))

(defconst os/linux (and (memq system-type '(gnu gnu/linux)) t))
(defconst os/bsd (and (memq system-type '(darwin berkeley-unix gnu/kfreebsd)) t))
(defconst os/win (and (memq system-type '(cygwin windows-nt ms-dos)) t))
(defconst os/mac (eq system-type 'darwin))

;; Should return x86_64, aarch64, armhf, ...
(defconst sys/arch (intern (car (split-string system-configuration "-"))))

(defconst emacs/features
  (mapcar #'intern
          (mapcar (apply-partially #'string-replace "_" "-")
                  (mapcar #'downcase (split-string system-configuration-features))))
  "List of symbols representing Emacs' enabled features.
Compiled from the `system-configuration-features'.")

(defcustom minemacs-fonts nil
  "Fonts to use within MinEmacs."
  :group 'minemacs
  :type '(plist
          (:font-family string)
          (:font-size natnum)
          (:unicode-font-family string)
          (:variable-pitch-font-family string)
          (:variable-pitch-font-size natnum)))

(defcustom minemacs-leader-key "SPC"
  "MinEmacs leader key."
  :group 'minemacs
  :type 'string)

(defcustom minemacs-localleader-key "SPC m"
  "MinEmacs local leader (a.k.a. mode specific) key sequence."
  :group 'minemacs
  :type 'string)

(defcustom minemacs-global-leader-prefix "C-SPC"
  "MinEmacs general leader key."
  :group 'minemacs
  :type 'string)

(defcustom minemacs-global-mode-prefix "C-SPC m"
  "MinEmacs general local leader (a.k.a. mode specific) key sequence."
  :group 'minemacs
  :type 'string)

(defcustom minemacs-theme 'doom-one-light
  "The theme of MinEmacs."
  :group 'minemacs
  :type 'symbol)

(defcustom minemacs-after-set-fonts-hook nil
  "Runs after setting MinEmacs fonts, runs at the end of `+set-fonts'."
  :group 'minemacs
  :type 'hook)

(defcustom minemacs-after-load-theme-hook nil
  "Runs after loading MinEmacs theme, runs at the end of `+load-theme'."
  :group 'minemacs
  :type 'hook)

(defcustom minemacs-after-startup-hook nil
  "This hook will be run after loading Emacs.

MinEmacs hooks will be run in this order:
1. `minemacs-after-startup-hook'
2. `minemacs-lazy-hook'"
  :group 'minemacs
  :type 'hook)

(defcustom minemacs-lazy-hook nil
  "This hook will be run after loading Emacs, with laziness.

MinEmacs hooks will be run in this order:
1. `minemacs-after-startup-hook'
2. `minemacs-lazy-hook'"
  :group 'minemacs
  :type 'hook)

;; Setup default fonts (depending on the OS)
(let ((mono-font (cond (os/linux "monospace")
                       (os/win "Lucida Console")
                       (os/mac "monospace")))
      (varp-font (cond (os/linux "monospace")
                       (os/win "Tahoma")
                       (os/mac "monospace"))))
  (defconst minemacs-default-fonts
    `(:font-family ,mono-font
      :font-size 14
      :unicode-font-family nil
      :variable-pitch-font-family ,varp-font
      :variable-pitch-font-size 14)
    "Default fonts of MinEmacs."))

(defcustom +env-save-vars
  '("PATH" "MANPATH" "CMAKE_PREFIX_PATH" "PKG_CONFIG_PATH" "LSP_USE_PLISTS")
  "List of the environment variables to saved by `+env-save'.
You need to run Emacs from terminal to get the environment variables.
MinEmacs then save them when calling `+env-save' to be used in GUI sessions as well."
  :group 'minemacs
  :type '(repeat string))


(provide 'me-vars)
