;;; me-daemon.el --- Daemon -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;; Email (mu4e)
(when (and (memq 'me-email minemacs-modules) (not (+package-disabled-p 'mu4e)))
  (+lazy!
   (when (and +mu4e-available-p +mu4e-auto-start (require 'mu4e nil :noerror))
     (defvar +daemon--mu4e-persist-timer
       (run-at-time
        (* 10) ;; 10s
        (* 60 3) ;; 3min
        (lambda ()
          (unless (or (+lockedp 'mu) (mu4e-running-p))
            (+info! "Starting `mu4e' in background.")
            (let ((inhibit-message t))
              (mu4e t)))))))))


(provide 'me-daemon)

;;; me-daemon.el ends here
