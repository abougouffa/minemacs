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

(dolist (module '("minemacs-bootstrap.el"
                  "minemacs-defaults.el"
                  "minemacs-evil.el"
                  "minemacs-utils.el"
                  "minemacs-keybindings.el"
                  "minemacs-ui.el"
                  "minemacs-editor.el"
                  "minemacs-completion.el"
                  "minemacs-prog.el"
                  "minemacs-email.el"
                  "minemacs-lisp.el"
                  "minemacs-vc.el"
                  "minemacs-pdf.el"))
  (load module))
