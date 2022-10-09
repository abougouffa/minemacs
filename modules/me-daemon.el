;; -*- lexical-binding: t; -*-


(defun me-daemon--start ()
  (with-eval-after-load 'minemacs-loaded-stage-2
    (unless (daemonp)
      (let ((inhibit-message t))
        (me-info! "Starting Emacs daemon in background.")
        (server-start)))))


(defun me-daemon--setup-background-apps ()
  (with-eval-after-load 'minemacs-loaded-stage-1
    ;; mu4e
    (when (require 'mu4e nil t)
      (run-at-time
       nil
       (* 60 3) ;; Check each 3m
       (lambda ()
         (unless (mu4e-running-p)
           (let ((inhibit-message t))
             (mu4e--start)
             (me-info! "Started `mu4e' in background."))))))

    ;; RSS
    (when (require 'elfeed nil t)
      (run-at-time
       (* 60 5)
       (* 60 60 3)
       (lambda ()
         (let ((inhibit-message t))
           (me-info! "Updating RSS feed.")
           (elfeed-update))))))) ;; Check every 2h


;; At daemon startup
(add-hook 'emacs-startup-hook #'me-daemon--setup-background-apps)

(unless (daemonp)
  (add-hook 'emacs-startup-hook #'me-daemon--start))


(provide 'me-daemon)
