;; me-vars.el --- MinEmacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>

;;; MinEmacs directories

(defconst minemacs-config-dir
  (file-name-as-directory
   (or (getenv "MINEMACS_DIR")
       (getenv "MINEMACSDIR")
       "~/.minemacs.d/"))
  "MinEmacs user customization directory.")

(defconst minemacs-debug
  (not (null (or (getenv "MINEMACS_DEBUG") init-file-debug)))
  "MinEmacs is started in debug mode.")

(defconst minemacs-verbose
  (not (null (or (getenv "MINEMACS_VERBOSE") minemacs-debug)))
  "MinEmacs is started in verbose mode.")

(defconst minemacs-not-lazy
  (or (daemonp) (not (null (getenv "MINEMACS_NOT_LAZY"))))
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
4 - `+debug!' (reserved)")

(defconst minemacs-splash-keep
  (not (null (getenv "MINEMACS_SPLASH_KEEP")))
  "MinEmacs is started in keep splash screen mode.")

;; Derive the root directory from this file path
(defconst minemacs-root-dir
  (abbreviate-file-name
   (file-name-parent-directory
    (file-name-directory (file-truename load-file-name)))))
(defconst minemacs-core-dir (concat minemacs-root-dir "core/"))
(defconst minemacs-elisp-dir (concat minemacs-root-dir "elisp/"))
(defconst minemacs-modules-dir (concat minemacs-root-dir "modules/"))
(defconst minemacs-extras-dir (concat minemacs-modules-dir "extras/"))
(defconst minemacs-local-dir (concat minemacs-root-dir "local/"))
(defconst minemacs-cache-dir (concat minemacs-local-dir "cache/"))
(defconst minemacs-autoloads-file (concat minemacs-core-dir "me-autoloads.el"))

(defconst os/linux (not (null (memq system-type '(gnu gnu/linux)))))
(defconst os/bsd (not (null (memq system-type '(darwin berkeley-unix)))))
(defconst os/win (not (null (memq system-type '(cygwin windows-nt ms-dos)))))
(defconst os/mac (eq system-type 'darwin))

;; Should return x86_64, aarch64, ...
(defconst sys/arch (intern (substring system-configuration 0 (string-search "-" system-configuration))))

(defconst emacs/features
  (mapcar #'intern
          (mapcar (apply-partially #'string-replace "_" "-")
                  (mapcar #'downcase (split-string system-configuration-features))))
  "List of symbols representing Emacs' enabled features.
Compiled from the `system-configuration-features'.")

(defcustom minemacs-fonts nil
  "Fonts to use within MinEmacs.")

(defcustom minemacs-leader-key "SPC"
  "MinEmacs leader key.")

(defcustom minemacs-localleader-key "SPC m"
  "MinEmacs local leader (a.k.a. mode specific) key sequence.")

(defcustom minemacs-global-leader-prefix "C-SPC"
  "MinEmacs general leader key.")

(defcustom minemacs-global-mode-prefix "C-SPC m"
  "MinEmacs general local leader (a.k.a. mode specific) key sequence.")

(defcustom minemacs-theme 'doom-one-light
  "The theme of MinEmacs")

(defcustom minemacs-before-user-config-hook nil
  "This hook will be run after loading modules and before loading user config.

MinEmacs hooks will be run in this order:
1. minemacs-before-user-config-hook
2. minemacs-after-startup-hook
3. minemacs-lazy-hook"
  :group 'minemacs
  :type 'hook)

(defcustom minemacs-after-startup-hook nil
  "This hook will be run after loading Emacs.

MinEmacs hooks will be run in this order:
1. minemacs-before-user-config-hook
2. minemacs-after-startup-hook
3. minemacs-lazy-hook"
  :group 'minemacs
  :type 'hook)

(defcustom minemacs-lazy-hook nil
  "This hook will be run after loading Emacs, with laziness.

MinEmacs hooks will be run in this order:
1. minemacs-before-user-config-hook
2. minemacs-after-startup-hook
3. minemacs-lazy-hook"
  :group 'minemacs
  :type 'hook)

(let ((mono-font (cond (os/linux "monospace")
                       (os/win "Lucida Console")
                       (os/mac "monospace")))
      (varp-font (cond (os/linux "monospace")
                       (os/win "Tahoma")
                       (os/mac "monospace"))))
  (defconst minemacs-default-fonts
    `(:font-family ,mono-font
      :font-size 14
      :variable-pitch-font-family ,varp-font
      :variable-pitch-font-size 14)
    "Default fonts of MinEmacs."))

(defvar minemacs-deps-executables
  '(grep find tar zip unzip zstd bzip2 gzip file ssh fd rg curl wget (xsel xclip)
    mu msmtp mbsync git mpv valgrind (gcc clang) (gdb lldb) cmake make clang-format aspell
    (python3 python) rosbag cargo maxima octave fortune (xelatex pdflatex lualatex) (latexmk tectonic)
    (clangd ccls) (pyls pylsp pyright) dot-language-server rust-analyzer prettier
    cmake-language-server bash-language-server docker-langserver yaml-language-server
    vscode-json-languageserver marksman digestif wkhtmltopdf txt2html)
  "A list of programs I use within Emacs.")

(defvar +env-save-vars
  '("PATH" "MANPATH" "CMAKE_PREFIX_PATH" "PKG_CONFIG_PATH" "LSP_USE_PLISTS")
  "List of environment variables saved by `+env-save'.
You need to run Emacs from terminal to get the environment variables.
MinEmacs then save them to be used in GUI sessions as well.")


(provide 'me-vars)
