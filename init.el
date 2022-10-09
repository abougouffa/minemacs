;; init.el --- MinEmacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>

(defvar minemacs-core-modules
  '(defaults bootstrap core-ui keybindings evil completion gc splash))

(defvar minemacs-modules
  '(ui editor vc prog lisp data
    org notes email docs spell
    files tools biblio daemon rss ros eaf math))

;; The modules.el file can override minemacs-modules and minemacs-core-modules
(let ((mods (expand-file-name "modules.el" minemacs-config-dir)))
  (when (file-exists-p mods)
    (me-log! "Loading modules file from \"%s\"" mods)
    (load mods nil (not init-file-debug))))

(defun minemacs-reload (&optional without-core)
  "Reload all configuration, including user's config.el."
  (interactive)
  ;; Set fonts early
  (run-at-time nil nil (lambda () (me-set-fonts)))
  ;; Core modules
  (unless without-core
    (dolist (module minemacs-core-modules)
      (me-log! "Loading core module \"%s\"" module)
      (load (expand-file-name (format "core/me-%s.el" module) user-emacs-directory)
            nil (not init-file-debug))))

  ;; Modules
  (dolist (module minemacs-modules)
    (me-log! "Loading module \"%s\"" module)
    (load (expand-file-name (format "modules/me-%s.el" module) user-emacs-directory)
          nil (not init-file-debug)))

  (when (and custom-file (file-exists-p custom-file))
    (me-log! "Loafing user customs from custom.el")
    (load custom-file nil (not init-file-debug)))

  ;; Load user config when available
  (let ((user-config (expand-file-name "config.el" minemacs-config-dir)))
    (when (file-exists-p user-config)
      (me-log! "Loading user config file from \"%s\"" user-config)
      (load user-config nil (not init-file-debug)))))

;; Load for the first time
(minemacs-reload)

(me-log! "Loaded early-config.el")
