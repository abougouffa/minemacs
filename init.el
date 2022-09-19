;;; init.el --- MinEmacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <hacko@laptop>

;; Profile emacs startup
(add-hook
 'emacs-startup-hook
 (lambda ()
   (message "MinEmacs loaded in %s." (emacs-init-time))))

(defconst IS-LINUX (memq system-type '(gnu gnu/linux)))
(defconst IS-BSD   (memq system-type '(darwin berkeley-unix)))
(defconst IS-MAC   (eq system-type 'darwin))
(defconst IS-WIN   (memq system-type '(cygwin windwos-nt ms-dos)))

(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

(dolist (module '("bootstrap.el"
                  "defaults.el"
                  "evil.el"
                  "utils.el"
                  "keybindings.el"
                  "ui.el"
                  "editor.el"
                  "completion.el"
                  "prog.el"
                  "email.el"
                  "lisp.el"
                  "pdf.el"))
  (load module))
