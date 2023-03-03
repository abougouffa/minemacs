;;; me-daemon.el --- Daemon -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>

;; Email (mu4e)
(+lazy-when! (featurep 'me-email)
  (when (require 'mu4e nil t)
    (unless (mu4e-running-p)
      (let ((inhibit-message t))
        (mu4e t)
        (+info! "Started `mu4e' in background.")))))

;; RSS (elfeed)
(+lazy-when! (featurep 'me-rss)
  (run-at-time
   (* 60 5) ;; 5min
   (* 60 60) ;; 1h
   (lambda ()
     (+info! "Updating RSS feed.")
     (let ((inhibit-message t))
       (elfeed-update)))))

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

(add-hook
 'server-after-make-frame-hook
 (defun +daemon--reload-battery-mode-once-h ()
   (when (and (display-graphic-p)
              (bound-and-true-p display-battery-mode))
     (display-battery-mode -1)
     (display-battery-mode 1)
     (remove-hook 'server-after-make-frame-hook
                  #'+daemon--reload-battery-mode-once-h))))


(provide 'me-daemon)
