;; me-vars.el --- MinEmacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2022-10-02
;; Last modified: 2025-07-05

;;; Commentary:

;;; Code:

;;; MinEmacs groups

(defgroup minemacs nil "MinEmacs specific functionalities." :group 'emacs)
(defgroup minemacs-apps nil "MinEmacs applications." :group 'minemacs)
(defgroup minemacs-binary nil "MinEmacs binary files." :group 'minemacs)
(defgroup minemacs-buffer nil "MinEmacs buffer stuff." :group 'minemacs)
(defgroup minemacs-completion nil "Completion related stuff." :group 'minemacs)
(defgroup minemacs-core nil "MinEmacs core tweaks." :group 'minemacs)
(defgroup minemacs-edit nil "MinEmacs editor tweaks." :group 'minemacs)
(defgroup minemacs-keybinding nil "MinEmacs keybinding." :group 'minemacs)
(defgroup minemacs-org nil "MinEmacs `org-mode' tweaks." :group 'minemacs)
(defgroup minemacs-prog nil "MinEmacs programming stuff." :group 'minemacs)
(defgroup minemacs-project nil "MinEmacs project stuff." :group 'minemacs)
(defgroup minemacs-ui nil "MinEmacs UI tweaks." :group 'minemacs)
(defgroup minemacs-utils nil "MinEmacs utility functions." :group 'minemacs)
(defgroup minemacs-vc nil "MinEmacs version control stuff." :group 'minemacs)

;;; MinEmacs directories

(defconst minemacs-ignore-user-config
  (when-let* ((ignores (getenv "MINEMACS_IGNORE_USER_CONFIG"))
              (ignores (and ignores (downcase ignores))))
    (if (equal ignores "all")
        '(early-config init-tweaks modules config local/early-config local/init-tweaks local/modules local/config)
      (mapcar #'intern (split-string ignores))))
  "Ignore loading these user configuration files.
Accepted values are: early-config, init-tweaks, modules, config,
local/early-config, local/init-tweaks, local/modules and local/config.
This list is automatically constructed from the space-separated values in the
environment variable \"$MINEMACS_IGNORE_USER_CONFIG\".")

(defconst minemacs-debug-p
  (and (or (getenv "MINEMACS_DEBUG") init-file-debug) t)
  "MinEmacs is started in debug mode.")

(defconst minemacs-verbose-p
  (and (or (getenv "MINEMACS_VERBOSE") minemacs-debug-p) t)
  "MinEmacs is started in verbose mode.")

(defconst minemacs-always-demand-p
  (and (getenv "MINEMACS_ALWAYS_DEMAND") t)
  "Load all packages immediately, do not defer any package.")

(defconst minemacs-not-lazy-p
  (or minemacs-always-demand-p (daemonp) (and (getenv "MINEMACS_NOT_LAZY") t))
  "Load lazy packages (minemacs-lazy-hook) immediately.")

(defconst minemacs-builtin-only-p
  (and (getenv "MINEMACS_BUILTIN_ONLY") t)
  "Load only built-in packages, no need to install any external package.")

(defconst minemacs-load-all-modules-p
  (and (getenv "MINEMACS_LOAD_ALL_MODULES") t)
  "Force loading all MinEmacs modules.")

(defconst minemacs-no-proxies-p
  (and (getenv "MINEMACS_NO_PROXIES") t)
  "Disable proxies in `minemacs-proxies'.")

(defcustom minemacs-msg-level
  (let ((level (string-to-number (or (getenv "MINEMACS_MSG_LEVEL") ""))))
    (cond (minemacs-verbose-p 4)
          ((> level 0) level)
          (t 1)))
  "Level of printed messages.
1 - `+error!'
2 - `+info!'
3 - `+log!'
4 - `+debug!'"
  :group 'minemacs-core
  :type '(choice
          (const :tag "Error" 1)
          (const :tag "Info" 2)
          (const :tag "Log" 3)
          (const :tag "Debug" 4)))

;; Derive the root directory from this file path
(defconst minemacs-root-dir (abbreviate-file-name (file-name-directory (directory-file-name (file-name-directory (file-truename load-file-name))))))
(defconst minemacs-core-dir (concat minemacs-root-dir "core/"))
(defconst minemacs-assets-dir (concat minemacs-root-dir "assets/"))
(defconst minemacs-elisp-dir (concat minemacs-root-dir "elisp/"))
(defconst minemacs-modules-dir (concat minemacs-root-dir "modules/"))
(defconst minemacs-obsolete-modules-dir (concat minemacs-modules-dir "obsolete/"))
(defconst minemacs-on-demand-modules-dir (concat minemacs-modules-dir "on-demand/"))
(defconst minemacs-local-dir (concat minemacs-root-dir "local/"))
(defconst minemacs-cache-dir (concat minemacs-local-dir "cache/"))
(defconst minemacs-loaddefs-file (concat minemacs-core-dir "me-loaddefs.el"))
(defconst minemacs-config-dir (file-name-as-directory
                               (or (getenv "MINEMACS_DIR")
                                   (getenv "MINEMACSDIR")
                                   (and (file-directory-p "~/.minemacs.d/") "~/.minemacs.d/")
                                   (concat minemacs-root-dir "user-config/")))
  "MinEmacs user customization directory.")

(setq user-emacs-directory minemacs-local-dir)

(defconst minemacs-default-org-dir
  (or (getenv "MINEMACS_ORG_DIR")
      (seq-find #'file-directory-p
                (let (dir-candidates)
                  (dolist (dir `(,(expand-file-name "org" minemacs-config-dir)
                                 ,(expand-file-name "~/org")
                                 ,@(when-let* ((dir (getenv "XDG_DOCUMENTS_DIR"))) (list (expand-file-name "org" dir)))
                                 ,(expand-file-name "~/Documents/org")))
                    (dolist (func '(downcase upcase capitalize))
                      (push
                       (expand-file-name (funcall func (file-name-nondirectory dir)) (file-name-directory dir))
                       dir-candidates)))
                  dir-candidates))
      "~/org") ; The default value for `org-directory'
  "The default directory to use for `org-directory'.
If environment variable \"MINEMACS_ORG_DIR\" is defined, it gets used,
else, the first existing directory of the following list will be
used (all small case, or title case):

- `minemacs-config-dir'/{Org,org,ORG}
- $HOME/{Org,org,ORG}
- $XDG_DOCUMENTS_DIR/{Org,org,ORG}
- $HOME/Documents/{Org,org,ORG}.")

(defconst minemacs-started-with-extra-args-p (and (cdr command-line-args) t) "Has Emacs been started with extras arguments? like a file name or so.")

;; Register some Emacs build options as features
(setq features
      (append
       features
       (list
        (intern (concat "arch/" (car (split-string system-configuration "-"))))
        (cond ((eq system-type 'android) 'os/android)
              ((memq system-type '(gnu gnu/linux)) 'os/linux)
              ((memq system-type '(berkeley-unix gnu/kfreebsd)) 'os/bsd)
              ((memq system-type '(cygwin windows-nt ms-dos)) 'os/win)
              ((eq system-type 'darwin) 'os/mac)
              (t 'os/unknown)))
       (mapcar #'intern
               (mapcar (apply-partially #'string-replace "_" "-")
                       (mapcar (apply-partially #'concat "feat/") (mapcar #'downcase (split-string system-configuration-features)))))))

(defcustom minemacs-theme 'doom-one-light
  "The theme of MinEmacs."
  :group 'minemacs-ui
  :type 'symbol)

(defcustom minemacs-disabled-packages nil
  "List of packages to be disabled when loading MinEmacs modules.
This can be useful if you want to enable a module but you don't want a package
of being enabled."
  :group 'minemacs-core
  :type '(repeat symbol))

(defvar minemacs-configured-packages nil
  "List of packages installed and configured by MinEmacs during startup.")

(defcustom minemacs-after-setup-fonts-hook nil
  "Runs after setting MinEmacs fonts, runs at the end of `+setup-fonts'."
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

;; TODO: Maybe replace with `url-proxy-services' (if the last works with shell commands)
(defcustom minemacs-proxies nil
  "MinEmacs proxies.

Example, set it to:

\\='((\"no\" . \"localhost,127.0.0.1,.local,.mylocaltld\")
  (\"ftp\" . \"http://myproxy.local:8080/\")
  (\"http\" . \"http://myproxy.local:8080/\")
  (\"https\" . \"http://myproxy.local:8080/\")))

These will set the environment variables \"no_proxy\", \"ftp_proxy\", ...

When set in \"early-config.el\" or in \"init-tweaks.el\", MinEmacs will enable
it automatically."
  :group 'minemacs-core
  :type '(repeat (cons string string)))

(defcustom minemacs-modules
  '(;; me-ai
    ;; me-calendar
    me-checkers
    me-completion
    me-debug
    me-docs
    me-editor
    me-emacs-lisp
    ;; me-email
    ;; me-embedded
    ;; me-experimental
    me-extra
    me-files
    ;; me-fun
    ;; me-lifestyle
    ;; me-media
    me-multi-cursors
    me-natural-langs
    me-nav
    me-notes
    me-org
    me-prog
    me-project
    ;; me-robot
    ;; me-rss
    ;; me-services
    me-snippets
    ;; me-tags
    me-tools
    me-ui
    me-vc)
  "MinEmacs enabled modules."
  :group 'minemacs-core
  :type '(repeat symbol))

(defvaralias 'minemacs-build-functions-hook 'minemacs-build-functions)
(defvar minemacs-build-functions nil
  "Special hook for build functions that are run after completing package updates.")

(defcustom +env-file (concat minemacs-local-dir "system-env.el")
  "The file in which the environment variables will be saved."
  :group 'minemacs-core
  :type 'file)

;; Inspired by Doom Emacs
(defcustom +env-deny-vars
  '(;; Unix/shell state that shouldn't be persisted
    "^HOME$" "^\\(OLD\\)?PWD$" "^SHLVL$" "^PS1$" "^R?PROMPT$" "^TERM\\(CAP\\)?$"
    "^USER$" "^GIT_CONFIG" "^INSIDE_EMACS$" "^SESSION_MANAGER$" "^_$"
    "^JOURNAL_STREAM$" "^INVOCATION_ID$" "^MANAGERPID$" "^SYSTEMD_EXEC_PID$"
    "^DESKTOP_STARTUP_ID$" "^LS_?COLORS$" "^$"
    ;; Python virtual environment
    "^VIRTUAL_ENV$"
    ;; Gnome session
    "^GNOME_\\(DESKTOP_SESSION_ID\\|SETUP_DISPLAY\\|SHELL_SESSION_MODE\\)$"
    ;; KDE session
    "^KDE_\\(FULL_SESSION\\|APPLICATIONS_.*\\|SESSION_\\(UID\\|VERSION\\)\\)$"
    ;; X server, Wayland, or services' env that shouldn't be persisted
    "^DISPLAY$" "^WAYLAND_DISPLAY" "^DBUS_SESSION_BUS_ADDRESS$" "^XAUTHORITY$"
    "^WINDOWID$" "^GIO_LAUNCHED_DESKTOP_FILE_PID$"
    ;; Windows+WSL envvars that shouldn't be persisted
    "^WSL_INTEROP$"
    ;; XDG variables that are best not persisted.
    "^XDG_CURRENT_DESKTOP$" "^XDG_RUNTIME_DIR$"
    "^XDG_\\(VTNR\\|SEAT\\|SESSION_\\(TYPE\\|CLASS\\|ID\\|PATH\\|DESKTOP\\)\\)"
    ;; Socket envvars, like I3SOCK, GREETD_SOCK, SEATD_SOCK, SWAYSOCK, etc.
    "SOCK$"
    ;; SSH and GPG variables that could quickly become stale if persisted.
    "^SSH_\\(AUTH_SOCK\\|AGENT_PID\\)$" "^\\(SSH\\|GPG\\)_TTY$" "^GPG_AGENT_INFO$"
    ;; Tmux session
    "^TMUX$"
    ;; MinEmacs envvars
    "^MINEMACS_")
  "Environment variables to omit.
Each string is a regexp, matched against variable names to omit from
`+env-file' when saving evnironment variables in `+env-save'."
  :group 'minemacs-core
  :type '(repeat regexp))

(defvar minemacs-on-demand-modules-alist nil "List of extra on-demand modules.")

(defcustom minemacs-on-demand-enable-plist
  '( :auto-mode no-ask
     :interpreter-mode no-ask
     :magic-mode no-ask
     :magic-fallback-mode no-ask
     :companion-packages no-ask)
  "Enable loading on-demand packages when needed."
  :group 'minemacs-core
  :type 'plist)

(defface +goto-addr-url-face '((t :italic t :underline t))
  "Face for URLs, I prefer keeping the original face and add underline and italic."
  :group 'goto-addr)

(defcustom +clang-format-command "clang-format"
  "The \"clang-format\" command to use by default.
This allows us to use a specific Clang-format version (like
\"clang-format-12\"). This command will be used in
`+editorconfig-guess-style-from-clang-format',
`reformatter', `apheleia', etc."
  :group 'minemacs-prog)


(provide 'me-vars)
;;; me-vars.el ends here
