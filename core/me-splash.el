;; me-splash.el --- MinEmacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa and contributors

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

;; Adapted from: github.com/rougier/emacs-splash

(setq inhibit-startup-screen t)
(defvar minemacs-splash-buffer-name "*minemacs-splash*")

(defun minemacs-splash ()
  "MinEmacs splash screen."
  ;; If there are buffer associated with filenames, we don't show splash screen.
  (unless (seq-filter #'buffer-file-name (buffer-list))
    (let* ((buffer (get-buffer-create minemacs-splash-buffer-name))
           (height (- (window-body-height nil) 1))
           (padding-center (min 5 (- (/ height 3) 1)))
           (padding-bottom (min 2 (- height (/ height 3) 3))))
      (with-current-buffer buffer
        (erase-buffer)
        ;; Buffer local settings
        (setq-local cursor-type nil
                    vertical-scroll-bar nil
                    horizontal-scroll-bar nil)

        ;; Vertical padding to center
        (insert-char ?\n padding-center)

        (when (display-graphic-p)
          (insert-image (create-image (concat minemacs-assets-dir "images/minemacs-small.png")))
          (insert-char ?\n))

        ;; Central text
        (insert-char ?\s 10)
        (insert (propertize "MinEmacs" 'face 'bold))
        (insert-char ?\n)
        (insert-char ?\s 10)
        (insert (propertize
                 (format "Running GNU Emacs %s%s"
                         emacs-version
                         (if emacs-repository-version
                             (format " (%s)" (substring emacs-repository-version 0 10))
                           ""))
                 'face 'shadow))

        ;; Bootstrapping
        (unless (file-exists-p (concat minemacs-local-dir "straight/repos/straight.el/bootstrap.el"))
          (insert-char ?\n)
          (insert-char ?\s 10)
          (insert (propertize "You are running MinEmacs for the first time."
                              'face 'warning))
          (insert-char ?\n)
          (insert-char ?\s 10)
          (insert (propertize "Please wait while MinEmacs installs the required packages."
                              'face 'warning)))

        ;; Vertical padding to bottom
        (insert-char ?\n padding-bottom)

        ;; Copyright text
        (insert-char ?\n)
        (insert-char ?\s 10)
        (insert (propertize "Minimal Emacs configuration for daily use" 'face 'shadow))
        (insert-char ?\n)
        (insert-char ?\s 10)
        (insert-text-button "github.com/abougouffa/minemacs"
                            'action (lambda (_) (browse-url "https://github.com/abougouffa/minemacs"))
                            'help-echo "Visit MinEmacs repo"
                            'follow-link t)
        (insert-char ?\n)

        (goto-char 0)
        (read-only-mode t)

        (local-set-key (kbd "<escape>") (lambda () (interactive) (minemacs-splash-kill)))
        (local-set-key (kbd "q") (lambda () (interactive) (minemacs-splash-kill)))
        (local-set-key (kbd "<mouse-1>") 'mouse-set-point)
        (local-set-key (kbd "<mouse-2>") 'operate-this-button)

        (display-buffer-same-window buffer nil)))))

(defun minemacs-splash-kill ()
  (when (get-buffer minemacs-splash-buffer-name)
    (kill-buffer minemacs-splash-buffer-name)))

;; Display splash screen
(minemacs-splash)

;; Close splash screen automatically after Emacs gets loaded
(add-hook
 'emacs-startup-hook
 (defun +minemacs-splash--kill-h ()
   (run-at-time 0.5 nil #'minemacs-splash-kill))
 101)


(provide 'me-splash)

;;; me-splash.el ends here
