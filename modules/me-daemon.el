;;; me-daemon.el --- Daemon -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

;; Email (mu4e)
(+lazy-when! (memq 'me-email minemacs-modules)
  (when (and +mu4e-available-p +mu4e-auto-start (require 'mu4e nil :noerror))
    (defvar +daemon--mu4e-persist-timer
      (run-at-time
       (* 10) ;; 10s
       (* 60 3) ;; 3min
       (lambda ()
         (unless (or (+lockedp 'mu) (mu4e-running-p))
           (+info! "Starting `mu4e' in background.")
           (let ((inhibit-message t))
             (mu4e t))))))))

;; RSS (elfeed)
(+lazy-when! (memq 'me-rss minemacs-modules)
  (when (require 'elfeed nil :noerror)
    (run-at-time
     (* 60 5) ;; 5min
     (* 60 60) ;; 1h
     (lambda ()
       (+info! "Updating RSS feed.")
       (let ((inhibit-message t))
         (elfeed-update))))))


(provide 'me-daemon)

;;; me-daemon.el ends here
