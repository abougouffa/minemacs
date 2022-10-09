;; -*- lexical-binding: t; -*-


(defun me-daemon--setup-background-apps ()
  (with-eval-after-load 'minemacs-loaded
    ;; mu4e
    (run-at-time
     1
     nil
     (lambda ()
       (when (require 'mu4e nil t)
         (unless (mu4e-running-p)
           (let ((inhibit-message t))
             (mu4e :background)
             (me-info! "Started `mu4e' in background.")))))))

  (with-eval-after-load 'minemacs-loaded-stage-1
    ;; RSS
    (when (require 'elfeed nil t)
      (run-at-time
       (* 60 5)
       (* 60 60 3)
       (lambda ()
         (let ((inhibit-message t))
           (me-info! "Updating RSS feed.")
           (elfeed-update)))))) ;; Check every 2h

  (with-eval-after-load 'minemacs-loaded-stage-2
    (unless (daemonp)
      (let ((inhibit-message t))
        (me-info! "Starting Emacs daemon in background.")
        (server-start nil t)))))


;; At daemon startup
(add-hook 'emacs-startup-hook #'me-daemon--setup-background-apps)


(provide 'me-daemon)
