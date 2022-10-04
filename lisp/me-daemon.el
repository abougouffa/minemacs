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


(provide 'me-daemon)
