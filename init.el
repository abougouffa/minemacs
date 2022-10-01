;;; init.el --- MinEmacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <hacko@laptop>

;; Profile emacs startup
(add-hook
 'emacs-startup-hook
 (lambda ()
   (message "MinEmacs loaded in %s." (emacs-init-time))))

(defconst LINUX-P (memq system-type '(gnu gnu/linux)))
(defconst BSD-P (memq system-type '(darwin berkeley-unix)))
(defconst WIN-P (memq system-type '(cygwin windwos-nt ms-dos)))
(defconst MAC-P (eq system-type 'darwin))

(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

(defun minemacs-reload ()
  "Reload all configuration, including user's config.el."
  (interactive)
  (dolist (module '("bootstrap.el"
                    "utils.el"
                    "defaults.el"
                    "evil.el"
                    "keybindings.el"
                    "ui.el"
                    "editor.el"
                    "completion.el"
                    "email.el"
                    "prog.el"
                    "lisp.el"
                    "vc.el"
                    "pdf.el"))
    (load (concat "minemacs-" module)))

  ;; Load user config when available
  (let ((user-config (expand-file-name "config.el" user-emacs-directory)))
    (when (file-exists-p user-config)
      (load user-config))))

;; Load for the first time
(minemacs-reload)

(defcustom minemacs-first-input-hook ()
  "Transient hooks run before the first user input."
  :type 'hook
  :local 'permenant-local
  :group 'minemacs)

(defcustom minemacs-first-file-hook ()
  "Transient hooks run before the first interactively opened file."
  :type 'hook
  :local 'permenant-local
  :group 'minemacs)

(defcustom minemacs-first-buffer-hook ()
  "Transient hooks run before the first interactively opened buffer."
  :type 'hook
  :local 'permenant-local
  :group 'minemacs)

;; (run-hook-on 'minemacs-first-buffer-hook '(find-file-hook minemacs-switch-buffer-hook))
;; (run-hook-on 'minemacs-first-file-hook   '(find-file-hook dired-initial-position-hook))
;; (run-hook-on 'minemacs-first-input-hook  '(pre-command-hook))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((DISABLED-+realgud:launch-plist :program "${workspaceFolder}/build/debug/bin/view_trajectory" :args
                                     ("traj=/home/hacko/PhD/workspace-no/vo/orig/dso_results/ceiling/seq_15/424x240/fps_15.00/maxframes_5/poses.csv" "traj=/home/hacko/PhD/workspace-no/vo/orig/dso_results/frontal/seq_15/424x240/fps_15.00/maxframes_5/poses.csv"))
     (projectile-project-compilation-cmd . "cmake --build build/debug -- -j 8")
     (projectile-project-configure-cmd . "cmake -S . -B build/debug -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -G Ninja")
     (projectile-project-test-cmd . "cmake --build build/debug --target test")
     (projectile-project-run-cmd . "build/debug/dso_dataset")
     (projectile-project-name . "dso"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:font "Iosevka Fixed Curly Slab 15"))))
 '(fixed-pitch ((t (:inherit (default)))))
 '(fixed-pitch-serif ((t (:inherit (default)))))
 '(variable-pitch ((t (:font "Iosevka Curly Slab 15")))))
