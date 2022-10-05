;; -*- lexical-binding: t; -*-


(add-hook
 'emacs-startup-hook
 (lambda ()
   ;; After 2m, launch Emacs server if we aren't in daemon mode
   (run-at-time
    (* 60 2) nil
    (lambda ()
      (unless (daemonp)
        (let ((inhibit-message nil))
          (me-info! "Starting Emacs daemon in background.")
          (server-start)))))))


(when (daemonp)
  ;; At daemon startup
  (add-hook
   'emacs-startup-hook
   (defun me--daemon-startup ()
     ;; mu4e
     (when (require 'mu4e nil t)
       ;; Automatically start `mu4e' in background.
       (run-at-time
        (* 60 1) ;; Launch after 1m
        (* 60 3) ;; Check each 3m
        (lambda ()
          (unless (mu4e-running-p)
            (let ((inhibit-message nil))
              (mu4e--start)
              (me-info! "Started `mu4e' in background."))))))

     ;; RSS
     (when (and (require 'elfeed nil t) nil)
       (run-at-time nil (* 2 60 60) #'elfeed-update))))) ;; Check every 2h


(provide 'me-daemon)
