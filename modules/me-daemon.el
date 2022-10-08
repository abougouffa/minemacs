;; -*- lexical-binding: t; -*-


(defun me-daemon--start ()
  (run-at-time
   (* 60 2) nil
   (lambda ()
     (unless (daemonp)
       (let ((inhibit-message nil))
         (me-info! "Starting Emacs daemon in background.")
         (server-start))))))


(defun me-daemon--setup-background-apps ()
  (run-at-time
   (* 60 1) nil ;; After 2m
   (lambda ()
     ;; mu4e
     (when (require 'mu4e nil t)
       ;; Automatically start `mu4e' in background.
       (run-at-time
        nil
        (* 60 3) ;; Check each 3m
        (lambda ()
          (unless (mu4e-running-p)
            (let ((inhibit-message nil))
              (mu4e--start)
              (me-info! "Started `mu4e' in background."))))))

     ;; RSS
     (when (and (require 'elfeed nil t) nil)
       (run-at-time
        (* 60 5)
        (* 60 60 2)
        #'elfeed-update))))) ;; Check every 2h


;; At daemon startup
(add-hook 'minemacs-after-startup-hook #'me-daemon--setup-background-apps)

(unless (daemonp)
  (add-hook 'minemacs-after-startup-hook #'me-daemon--start))

;; minemacs-after-startup-hook

(provide 'me-daemon)
