;; me-vars.el --- MinEmacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>

;;; MinEmacs directories

(defconst minemacs-config-dir
  (file-name-as-directory
   (or (getenv "MINEMACS_DIR")
       "~/.minemacs.d/")))

(defconst minemacs-debug
  (not (null (or (getenv "MINEMACS_DEBUG") init-file-debug))))

(defconst minemacs-verbose
  (not (null (or (getenv "MINEMACS_VERBOSE") minemacs-debug))))

(defconst minemacs-splash-keep
  (not (null (getenv "MINEMACS_SPLASH_KEEP"))))

(defconst minemacs-root-dir (file-name-as-directory user-emacs-directory))
(defconst minemacs-core-dir (concat minemacs-root-dir "core/"))
(defconst minemacs-modules-dir (concat minemacs-root-dir "modules/"))

(defconst minemacs-local-dir (concat minemacs-root-dir "local/"))
(defconst minemacs-cache-dir (concat minemacs-local-dir "cache/"))
(defconst minemacs-autoloads-file (concat minemacs-core-dir "me-autoloads.el"))

;; Replace the default Emacs directory with the "local" directory
(setq user-emacs-directory minemacs-local-dir)

(defconst os/linux (not (null (memq system-type '(gnu gnu/linux)))))
(defconst os/bsd (not (null (memq system-type '(darwin berkeley-unix)))))
(defconst os/win (not (null (memq system-type '(cygwin windows-nt ms-dos)))))
(defconst os/mac (eq system-type 'darwin))

(defconst +emacs-features
  (mapcar #'intern
          (mapcar (apply-partially #'string-replace "_" "-")
                  (mapcar #'downcase (split-string system-configuration-features)))))

(defcustom minemacs-after-startup-hook nil
  "This hook will be run after loading Emacs."
  :group 'minemacs
  :type 'hook
  :local 'permenant-local)

(let ((mono-font (cond (os/linux "monospace")
                       (os/win "Lucida Console")
                       (os/mac "monospace")))
      (varp-font (cond (os/linux "monospace")
                       (os/win "Tahoma")
                       (os/mac "monospace"))))
  (defconst minemacs-default-fonts
    `(:font-family ,mono-font
      :font-size 15
      :variable-pitch-font-family ,varp-font
      :variable-pitch-font-size 15)))

(defvar minemacs-theme 'doom-one-light)

(defcustom minemacs-fonts nil
  "MinEmacs theme.")

(defcustom minemacs-leader-key "SPC"
  "Leader key.")

(defcustom minemacs-localleader-key "SPC m"
  "Localleader (mode specific) key.")

(defvar minemacs-deps-executables
  '(grep find tar zip unzip zstd bzip2 gzip file ssh fd rg curl wget (xsel xclip)
    mu msmtp mbsync git mpv valgrind (gcc clang) (gdb lldb) cmake make clang-format aspell
    (python3 python) rosbag cargo maxima octave fortune (xelatex pdflatex lualatex) (latexmk tectonic)
    (clangd ccls) (pyls pylsp pyright) dot-language-server rust-analyzer prettier
    cmake-language-server bash-language-server docker-langserver yaml-language-server
    vscode-json-languageserver marksman digestif)
  "A list of programs I use within Emacs.")

(defvar +env-save-vars
  '("PATH" "MANPATH" "CMAKE_PREFIX_PATH" "PKG_CONFIG_PATH")
  "List of environment variables saved by `+env-save'.
You need to run Emacs from terminal to get the environment variables.
MinEmacs then save them to be used in GUI sessions as well.")


(provide 'me-vars)
