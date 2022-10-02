;;; init.el --- MinEmacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <hacko@laptop>

;; Syncronization point!
;; Profile emacs startup and trigger `minemacs-loaded' 5s after loading Emacs
(add-hook
 'emacs-startup-hook
 (lambda ()
   (message "MinEmacs loaded in %s, loading lazy stuff." (emacs-init-time))
   (require 'minemacs-loaded)
   (dolist (fun minemacs-lazy-funs)
     (funcall fun))))

(defun minemacs-reload (&optional without-core)
  "Reload all configuration, including user's config.el."
  (interactive)
  ;; Core modules
  (unless without-core
    (dolist (module '("bootstrap" "defaults"))
      (load (concat module ".el") nil (not init-file-debug))))

  ;; Modules
  (dolist (module '("evil" "keybindings" "completion" "ui"
                    "editor" "version-control" "prog" "lisp"
                    "org" "email" "docs" "tools"))
    (load (expand-file-name (concat "lisp/" module ".el") user-emacs-directory) nil (not init-file-debug)))

  ;; Load user config when available
  (let ((user-config (expand-file-name "config.el" minemacs-config-dir)))
    (when (file-exists-p user-config)
      (load user-config))))

;; Load for the first time
(let ((inhibit-message))
  (minemacs-reload))
