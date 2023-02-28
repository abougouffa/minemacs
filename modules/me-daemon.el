;;; me-daemon.el --- Daemon -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


(defun +daemon--setup-background-apps ()
  (+deferred!
   ;; mu4e
   (when (featurep 'me-email)
     (+eval-when-idle-for! (if (daemonp) 0 5)
       (when (require 'mu4e nil t)
         (unless (mu4e-running-p)
           (let ((inhibit-message t))
             (mu4e t)
             (+info! "Started `mu4e' in background."))))))

   ;; RSS
   (when (featurep 'me-rss)
     (+eval-when-idle!
       (run-at-time
        (* 60 5) ;; 5min
        (* 60 60) ;; 1h
        (lambda ()
          (+info! "Updating RSS feed.")
          (let ((inhibit-message t))
            (elfeed-update))))))

   (+eval-when-idle!
     (require 'server)
     (unless (or (daemonp) (server-running-p))
       (let ((inhibit-message t))
         (+info! "Starting Emacs daemon in background.")
         (server-start nil t))))))

;; At daemon startup
(add-hook 'emacs-startup-hook #'+daemon--setup-background-apps)

;; Reload theme on Daemon
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
