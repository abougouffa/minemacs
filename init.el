;; init.el --- MinEmacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>

(defvar minemacs-modules
  '(keybindings
    evil completion ui
    editor vc prog lisp data
    org notes email docs spell
    files tools biblio daemon rss))

(defun minemacs-reload (&optional without-core)
  "Reload all configuration, including user's config.el."
  (interactive)
  ;; Core modules
  (unless without-core
    (dolist (module '("bootstrap" "defaults"))
      (me-log! "Loading core module \"%s\"" module)
      (require (intern (concat "me-" module)))))

  ;; Modules
  (dolist (module minemacs-modules)
    (me-log! "Loading module \"%s\"" module)
    (require (intern (concat "me-" (symbol-name module)))))

  ;; Load user config when available
  (let ((user-config (expand-file-name "config.el" minemacs-config-dir)))
    (when (file-exists-p user-config)
      (me-log! "Loading user config file from \"%s\"" user-config)
      (load user-config))))

;; Load for the first time
(minemacs-reload)

(me-log! "Loaded early-config.el")
