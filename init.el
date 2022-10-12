;; init.el --- MinEmacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>

(defvar minemacs-core-modules
  '(defaults splash bootstrap core-ui keybindings evil completion))

(defvar minemacs-modules
  '(ui editor vc prog lisp data
    org notes email docs spell
    files tools biblio daemon rss ros eaf math))

;; The modules.el file can override minemacs-modules and minemacs-core-modules
(let ((mods (expand-file-name "modules.el" minemacs-config-dir)))
  (when (file-exists-p mods)
    (me-log! "Loading modules file from \"%s\"" mods)
    (load mods nil (not minemacs-verbose))))

(defun minemacs-reload (&optional load-core-modules)
  "Reload all configuration, including user's config.el."
  (interactive)
  ;; Set default fonts early
  (run-at-time nil nil (lambda () (me-set-fonts)))

  ;; Core modules
  (when load-core-modules
    (dolist (module minemacs-core-modules)
      (me-log! "Loading core module \"%s\"" module)
      (load (expand-file-name (format "me-%s.el" module) minemacs-core-dir)
            nil (not minemacs-verbose))))

  ;; Modules
  (dolist (module minemacs-modules)
    (me-log! "Loading module \"%s\"" module)
    (load (expand-file-name (format "me-%s.el" module) minemacs-modules-dir)
          nil (not minemacs-verbose)))

  (when (and custom-file (file-exists-p custom-file))
    (me-log! "Loafing user customs from custom.el")
    (load custom-file nil (not minemacs-verbose)))

  ;; Load user config when available
  (let ((user-config (expand-file-name "config.el" minemacs-config-dir)))
    (when (file-exists-p user-config)
      (me-log! "Loading user config file from \"%s\"" user-config)
      (load user-config nil (not minemacs-verbose))))

  ;; Load GC module lastly
  (run-at-time
   5 nil
   (lambda ()
     (load (expand-file-name "me-gc.el" minemacs-core-dir)
           nil (not minemacs-verbose)))))

;; Load for the first time
(minemacs-reload :load-core-modules)

(me-log! "Loaded early-config.el")
