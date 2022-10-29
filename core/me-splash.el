;; me-splash.el --- MinEmacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


(require 'cl-lib)

;; Adapted from: https://github.com/rougier/emacs-splash

(setq inhibit-splash-screen t)
(defvar minemacs-splash-buffer-name "*minemacs-splash*")

(defun minemacs-splash-screen ()
  "MinEmacs splash screen"
  (let* ((splash-buffer (get-buffer-create minemacs-splash-buffer-name))
         (height (- (window-body-height nil) 1))
         (padding-center (min 5 (- (/ height 3) 1)))
         (padding-bottom (min 2 (- height (/ height 3) 3))))

    ;; If there are buffer associated with filenames,
    ;; we don't show splash screen.
    (if (eq 0 (length (cl-loop for buf in (buffer-list)
                               if (buffer-file-name buf)
                               collect (buffer-file-name buf))))
        (with-current-buffer splash-buffer
          (erase-buffer)

          ;; Buffer local settings
          (if (one-window-p)
              (setq-local mode-line-format nil))
          (setq-local cursor-type nil
                      vertical-scroll-bar nil
                      horizontal-scroll-bar nil)

          ;; Vertical padding to center
          (insert-char ?\n padding-center)

          ;; Central text
          (insert-char ?\s 10)
          (insert (propertize "MinEmacs" 'face 'bold))
          (insert "\n")
          (insert-char ?\s 10)
          (insert (propertize
                   (format "Running GNU Emacs %s%s"
                           emacs-version
                           (if emacs-repository-version
                               (format " (%s)" (substring emacs-repository-version 0 8))
                             ""))
                   'face 'shadow))
          ;; Bootstraping
          (unless (file-exists-p (expand-file-name "straight/repos/straight.el/bootstrap.el" minemacs-local-dir))
            (insert "\n")
            (insert-char ?\s 10)
            (insert (propertize "You are running MinEmacs for the first time."
                                'face 'warning))
            (insert "\n")
            (insert-char ?\s 10)
            (insert (propertize "Please wait while MinEmacs installs the required packages."
                                'face 'warning)))

          ;; Vertical padding to bottom
          (insert-char ?\n padding-bottom)

          ;; Copyright text
          (insert "\n")
          (insert-char ?\s 10)
          (insert (propertize "Minimal Emacs configuration for daily use" 'face 'shadow))
          (insert "\n")
          (insert-char ?\s 10)
          (insert-text-button "github.com/abougouffa/minemacs"
                              'action (lambda (_) (browse-url "https://github.com/abougouffa/minemacs"))
                              'help-echo "Visit MinEmacs repo"
                              'follow-link t)
          (insert "\n")

          (goto-char 0)
          (read-only-mode t)

          (let ((k))
            (local-set-key (kbd "<escape>")  (lambda () (interactive) (minemacs-splash-screen-kill)))
            (local-set-key (kbd "q")         (lambda () (interactive) (minemacs-splash-screen-kill)))
            (local-set-key (kbd "<mouse-1>") 'mouse-set-point)
            (local-set-key (kbd "<mouse-2>") 'operate-this-button))

          (display-buffer-same-window splash-buffer nil)))))

(defun minemacs-splash-screen-kill ()
  (when (get-buffer minemacs-splash-buffer-name)
    (kill-buffer minemacs-splash-buffer-name)))

;; Display splash screen
(minemacs-splash-screen)

;; Close splash screen automatically after Emacs gets loaded
(add-hook
 'emacs-startup-hook
 (defun +splash--kill-h ()
   (run-at-time 1 nil #'minemacs-splash-screen-kill)))


(provide 'me-splash)
