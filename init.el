;; init.el --- MinEmacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>

;; Syncronization point!
;; Profile emacs startup and trigger `minemacs-loaded' 5s after loading Emacs
(add-hook
 'emacs-startup-hook
 (lambda ()
   (message "MinEmacs loaded in %s, loading lazy stuff." (emacs-init-time))
   (run-at-time
    5 nil
    (lambda ()
      (require 'minemacs-loaded)
      (dolist (fun minemacs-lazy-funs)
        (funcall fun))))))

(defvar minemacs-modules
  '(keybindings
    evil completion ui
    editor vc
    prog lisp data
    org notes email docs spell tools))

(defun minemacs-reload (&optional without-core)
  "Reload all configuration, including user's config.el."
  (interactive)
  ;; Core modules
  (unless without-core
    (dolist (module '("bootstrap" "defaults"))
      (when init-file-debug
        (message "[MinEmacs] Loading core module \"%s\"" module))
      (require (intern (concat "me-" module)))))

  ;; Modules
  (dolist (module minemacs-modules)
    (when init-file-debug
      (message "[MinEmacs] Loading module \"%s\"" module))
    (require (intern (concat "me-" (symbol-name module)))))

  ;; Load user config when available
  (let ((user-config (expand-file-name "config.el" minemacs-config-dir)))
    (when (file-exists-p user-config)
      (load user-config))))

;; Load for the first time
(minemacs-reload)
