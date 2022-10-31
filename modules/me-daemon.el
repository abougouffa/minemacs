;; -*- lexical-binding: t; -*-


(defun +daemon--setup-background-apps ()
  (with-eval-after-load 'minemacs-loaded
    (+eval-when-idle!
     ;; mu4e
     (when (require 'mu4e nil t)
       (unless (mu4e-running-p)
         (let ((inhibit-message t))
           (mu4e t)
           (+info! "Started `mu4e' in background.")))))

    ;; RSS
    (+eval-when-idle!
     (run-at-time
      (* 60 5)
      (* 60 60 3)
      (lambda ()
        (let ((inhibit-message t))
          (+info! "Updating RSS feed.")
          (elfeed-update)))))

    (+eval-when-idle!
     (require 'server)
     (unless (or (daemonp) (server-running-p))
       (let ((inhibit-message t))
         (+info! "Starting Emacs daemon in background.")
         (server-start nil t))))))


;; At daemon startup
(add-hook 'emacs-startup-hook #'+daemon--setup-background-apps)

;; TTY specific stuff
(add-hook 'tty-setup-hook #'xterm-mouse-mode)

(use-package xclip ;; or clipetty for OSX
  :straight t
  :when os/linux
  :config
  (add-hook
   'tty-setup-hook
   (defun +xclip--enable-in-tty-h ()
     (with-demoted-errors "%s" (xclip-mode 1)))))


;; Reload theme on Daemon
(add-hook
 'server-after-make-frame-hook
 (defun +daemon--reload-theme-h ()
   (load-theme minemacs-theme t)))

(add-hook
 'server-after-make-frame-hook
 (defun +daemon--reload-battery-mode-h ()
   (when (and (display-graphic-p)
              (bound-and-true-p display-battery-mode))
     (display-battery-mode -1)
     (display-battery-mode 1)
     (remove-hook 'server-after-make-frame-hook
                  #'+daemon--reload-battery-mode-h))))


(provide 'me-daemon)
