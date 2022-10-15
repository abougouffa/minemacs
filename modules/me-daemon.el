;; -*- lexical-binding: t; -*-


(defun me-daemon--setup-background-apps ()
  (with-eval-after-load 'minemacs-loaded
    (me-eval-when-idle!
     ;; mu4e
     (when (require 'mu4e nil t)
       (unless (mu4e-running-p)
         (let ((inhibit-message t))
           (mu4e t)
           (me-info! "Started `mu4e' in background.")))))

    ;; RSS
    (me-eval-when-idle!
     (run-at-time
      (* 60 5)
      (* 60 60 3)
      (lambda ()
        (let ((inhibit-message t))
          (me-info! "Updating RSS feed.")
          (elfeed-update)))))

    (me-eval-when-idle!
     (unless (daemonp)
       (let ((inhibit-message t))
         (me-info! "Starting Emacs daemon in background.")
         (server-start nil t))))))


;; At daemon startup
(add-hook 'emacs-startup-hook #'me-daemon--setup-background-apps)

;; Reload theme on Daemon
(add-hook
 'server-after-make-frame-hook
 (lambda ()
   (load-theme 'doom-one-light t)))

(provide 'me-daemon)
