;; me-vars.el --- MinEmacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

;;; MinEmacs directories

(defgroup minemacs nil
  "MinEmacs specific functionalities.")

(defgroup minemacs-core nil
  "MinEmacs core tweaks."
  :group 'minemacs)

(defgroup minemacs-ui nil
  "MinEmacs UI tweaks."
  :group 'minemacs)

(defgroup minemacs-edit nil
  "MinEmacs editor tweaks."
  :group 'minemacs)

(defgroup minemacs-prog nil
  "MinEmacs programming stuff."
  :group 'minemacs)

(defgroup minemacs-keybinding nil
  "MinEmacs keybinding."
  :group 'minemacs)

(defgroup minemacs-utils nil
  "MinEmacs utility functions."
  :group 'minemacs)

(defconst minemacs-ignore-user-config
  (append
   (if (getenv "MINEMACS_IGNORE_USER_CONFIG")
       '(config modules early-config init-tweaks)
     (when (getenv "MINEMACS_IGNORE_CONFIG_EL") '(config))
     (when (getenv "MINEMACS_IGNORE_MODULES_EL") '(modules))
     (when (getenv "MINEMACS_IGNORE_CONFIG_EL") '(early-config))
     (when (getenv "MINEMACS_IGNORE_INIT_TWEAKS_EL") '(init-tweaks))))
  "Ignore loading these user configuration files.
Accepted values are: `config', `modules', `early-config' and `init-tweaks'.
This list is automatically constructed from the environment variables
\"$MINEMACS_IGNORE_*\".")

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
3 - `+log!'"
  :group 'minemacs-core
  :type '(choice
          (const :tag "Error" 1)
          (const :tag "Info" 2)
          (const :tag "Log" 3)
          (const :tag "Debug" 4)))

;; Derive the root directory from this file path
(defconst minemacs-root-dir
  (abbreviate-file-name
   (file-name-directory
    (directory-file-name
     (file-name-directory (file-truename load-file-name))))))
(defconst minemacs-core-dir (concat minemacs-root-dir "core/"))
(defconst minemacs-assets-dir (concat minemacs-root-dir "assets/"))
(defconst minemacs-elisp-dir (concat minemacs-root-dir "elisp/"))
(defconst minemacs-modules-dir (concat minemacs-root-dir "modules/"))
(defconst minemacs-extras-dir (concat minemacs-modules-dir "extras/"))
(defconst minemacs-local-dir (concat minemacs-root-dir "local/"))
(defconst minemacs-cache-dir (concat minemacs-local-dir "cache/"))
(defconst minemacs-loaddefs-file (concat minemacs-core-dir "me-loaddefs.el"))

(defconst os/linux (eq system-type 'gnu/linux) "Non-nil on GNU/Linux systems.")
(defconst os/bsd (and (memq system-type '(berkeley-unix gnu/kfreebsd)) t) "Non-nil on BSD systems.")
(defconst os/win (and (memq system-type '(cygwin windows-nt ms-dos)) t) "Non-nil on Windows systems.")
(defconst os/mac (eq system-type 'darwin) "Non-nil on MacOS systems.")

(when os/win
  (message "[DISCLAIMER]: MINEMACS HAS NOT BEEN TESTED ON WINDOWS, YOU SHOULD INVESTIGATE THE ISSUES YOU FACE!"))

(defconst sys/arch (intern (car (split-string system-configuration "-")))
  "The system's architecture read from `system-configuration'.
It return a symbol like `x86_64', `aarch64', `armhf', ...")

(defconst emacs/features
  (mapcar #'intern
          (mapcar (apply-partially #'string-replace "_" "-")
                  (mapcar #'downcase (split-string system-configuration-features))))
  "List of symbols representing Emacs' enabled features.
Compiled from the `system-configuration-features'.")

(defcustom minemacs-fonts nil
  "Fonts to use within MinEmacs."
  :group 'minemacs-ui
  :type '(plist
          (:font-family string)
          (:font-size natnum)
          (:unicode-font-family string)
          (:variable-pitch-font-family string)
          (:variable-pitch-font-size natnum)))

(defcustom minemacs-leader-key "SPC"
  "MinEmacs leader key."
  :group 'minemacs-keybinding
  :type 'string)

(defcustom minemacs-localleader-key "SPC m"
  "MinEmacs local leader (a.k.a. mode specific) key sequence."
  :group 'minemacs-keybinding
  :type 'string)

(defcustom minemacs-global-leader-prefix "C-SPC"
  "MinEmacs general leader key."
  :group 'minemacs-keybinding
  :type 'string)

(defcustom minemacs-global-mode-prefix "C-SPC m"
  "MinEmacs general local leader (a.k.a. mode specific) key sequence."
  :group 'minemacs-keybinding
  :type 'string)

(defcustom minemacs-theme 'doom-one-light
  "The theme of MinEmacs."
  :group 'minemacs-ui
  :type 'symbol)

(defcustom minemacs-disabled-packages nil
  "List of packages to be disabled when loading MinEmacs modules.
This can be useful if you want to enable a module but you don't want a package
of being enabled."
  :group 'minemacs-core
  :type '(list symbol))

(defvar minemacs-configured-packages nil
  "List of packages installed and configured by MinEmacs during startup.")

(defcustom minemacs-after-loading-modules-hook nil
  "This hook will be run after loading MinEmacs modules.
It is used internally to remove the `+use-package--check-if-disabled-a' advice
we set on `use-package' in `me-bootstrap'."
  :group 'minemacs-core
  :type 'hook)

(defcustom minemacs-after-set-fonts-hook nil
  "Runs after setting MinEmacs fonts, runs at the end of `+set-fonts'."
  :group 'minemacs-ui
  :type 'hook)

(defcustom minemacs-after-load-theme-hook nil
  "Runs after loading MinEmacs theme, runs at the end of `+load-theme'."
  :group 'minemacs-ui
  :type 'hook)

(defcustom minemacs-after-startup-hook nil
  "This hook will be run after loading Emacs.

MinEmacs hooks will be run in this order:
1. `minemacs-after-startup-hook'
2. `minemacs-lazy-hook'"
  :group 'minemacs-core
  :type 'hook)

(defcustom minemacs-lazy-hook nil
  "This hook will be run after loading Emacs, with laziness.

MinEmacs hooks will be run in this order:
1. `minemacs-after-startup-hook'
2. `minemacs-lazy-hook'"
  :group 'minemacs-core
  :type 'hook)

(defvar minemacs-build-functions nil
  "Special hook for build functions that are run after completing package updates.")
(defvaralias 'minemacs-build-functions-hook 'minemacs-build-functions)

;; Setup default fonts (depending on the OS)
(let ((mono-font (cond (os/linux "monospace")
                       (os/win "Lucida Console")
                       (os/mac "monospace")))
      (varp-font (cond (os/linux "monospace")
                       (os/win "Tahoma")
                       (os/mac "monospace"))))
  (defconst minemacs-default-fonts
    `(:font-family ,mono-font
      :font-size 13
      :unicode-font-family nil
      :variable-pitch-font-family ,varp-font
      :variable-pitch-font-size 13)
    "Default fonts of MinEmacs."))

(defcustom +env-file (concat minemacs-local-dir "system-env.el")
  "The file in which the environment variables will be saved."
  :group 'minemacs-core
  :type 'file)

;; List from Doom Emacs
(defcustom +env-deny-vars
  '(;; Unix/shell state that shouldn't be persisted
    "^HOME$" "^\\(OLD\\)?PWD$" "^SHLVL$" "^PS1$" "^R?PROMPT$" "^TERM\\(CAP\\)?$"
    "^USER$" "^GIT_CONFIG" "^INSIDE_EMACS$" "^SESSION_MANAGER$" "^_$"
    "^JOURNAL_STREAM$" "^INVOCATION_ID$" "^MANAGERPID$" "^SYSTEMD_EXEC_PID$"
    "^DESKTOP_STARTUP_ID$" "^LS_?COLORS$" "^$"
    ;; KDE session
    "^KDE_\\(FULL_SESSION\\|APPLICATIONS_.*\\|SESSION_\\(UID\\|VERSION\\)\\)$"
    ;; X server, Wayland, or services' env  that shouldn't be persisted
    "^DISPLAY$" "^WAYLAND_DISPLAY" "^DBUS_SESSION_BUS_ADDRESS$" "^XAUTHORITY$"
    ;; Windows+WSL envvars that shouldn't be persisted
    "^WSL_INTEROP$"
    ;; XDG variables that are best not persisted.
    "^XDG_CURRENT_DESKTOP$" "^XDG_RUNTIME_DIR$"
    "^XDG_\\(VTNR\\|SEAT\\|SESSION_\\(TYPE\\|CLASS\\|ID\\|PATH\\|DESKTOP\\)\\)"
    ;; Socket envvars, like I3SOCK, GREETD_SOCK, SEATD_SOCK, SWAYSOCK, etc.
    "SOCK$"
    ;; SSH and GPG variables that could quickly become stale if persisted.
    "^SSH_\\(AUTH_SOCK\\|AGENT_PID\\)$" "^\\(SSH\\|GPG\\)_TTY$"
    "^GPG_AGENT_INFO$"
    ;; MinEmacs envvars
    "^MINEMACS\\(_?DIR\\|_\\(ALPHA\\|DEBUG\\|VERBOSE\\|NOT_LAZY\\|MSG_LEVEL\\|IGNORE_.*\\)\\)$")
  "Environment variables to omit.
Each string is a regexp, matched against variable names to omit from
`+env-file' when saving evnironment variables in `+env-save'."
  :group 'minemacs-core
  :type '(repeat regexp))


(provide 'me-vars)

;;; me-vars.el ends here
