;;; me-daemon.el --- Daemon -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

;; Email (mu4e)
(+lazy-when! (memq 'me-email minemacs-modules)
  (when (and +mu4e-available-p (require 'mu4e nil t))
    (unless (mu4e-running-p)
      (+info! "Starting `mu4e' in background.")
      (let ((inhibit-message t))
        (mu4e t)))))

;; RSS (elfeed)
(+lazy-when! (memq 'me-rss minemacs-modules)
  (run-at-time
   (* 60 5) ;; 5min
   (* 60 60) ;; 1h
   (lambda ()
     (+info! "Updating RSS feed.")
     (let ((inhibit-message t))
       (elfeed-update)))))

;; When we start in a non-daemon Emacs, we start a server whe Emacs is idle.
(+lazy-unless! (daemonp)
  (require 'server) ; For using `server-running-p'
  (unless (server-running-p)
    (let ((inhibit-message t))
      (+info! "Starting Emacs daemon in background.")
      (server-start nil t))))

;; Reload theme when creating a frame on the daemon
(add-hook
 'server-after-make-frame-hook
 (defun +daemon--reload-theme-h ()
   (load-theme minemacs-theme t)))

(+hook-once! server-after-make-frame-hook
  (when (and (display-graphic-p)
             (bound-and-true-p display-battery-mode))
    (display-battery-mode -1)
    (display-battery-mode 1)))


(provide 'me-daemon)

;;; me-daemon.el ends here
