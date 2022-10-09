;; -*- lexical-binding: t; -*-


(require 'cl-lib)

;; Adapted from: https://github.com/rougier/emacs-splash

(setq inhibit-splash-screen t)
(defvar me-splash-buffer-name "*minemacs-splash*")

(defun me-splash-screen ()
  "MinEmacs splash screen"
  (interactive)
  (me-log! "Entering splash")
  (let* ((splash-buffer  (get-buffer-create me-splash-buffer-name))
         (height         (- (window-body-height nil) 1))
         (padding-center (min 7 (- (/ height 2) 1)))
         (padding-bottom (min 7 (- height (/ height 2) 3))))

    ;; If there are buffer associated with filenames,
    ;;  we don't show splash screen.
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
          (insert (propertize "Minimal Emacs configuration" 'face 'shadow))
          (insert "\n")

          ;; Vertical padding to bottom
          (insert-char ?\n padding-bottom)

          ;; Copyright text
          (insert "\n")
          (insert-char ?\s 10)
          (insert-text-button "github.com/abougouffa/minemacs"
                              'action (lambda (_) (browse-url "https://github.com/abougouffa/minemacs"))
                              'help-echo "Visit MinEmacs repo"
                              'follow-link t)
          (insert "\n")
          (insert-char ?\s 10)
          (insert (propertize (format "Running GNU Emacs %s (%s)"
                                      emacs-version
                                      (substring emacs-repository-version 0 10))
                              'face 'shadow))
          (insert "\n")

          (goto-char 0)
          (read-only-mode t)

          (local-set-key (kbd "C-[")       'me-splash-screen-kill)
          (local-set-key (kbd "<escape>")  'me-splash-screen-kill)
          (local-set-key (kbd "q")         'me-splash-screen-kill)
          (local-set-key (kbd "<mouse-1>") 'mouse-set-point)
          (local-set-key (kbd "<mouse-2>") 'operate-this-button)
          (display-buffer-same-window splash-buffer nil)))))

(defun me-splash-screen-kill ()
  "Kill the splash screen buffer (immediately)."
  (interactive)
  (when (get-buffer me-splash-buffer-name)
    (kill-buffer me-splash-buffer-name)))

;; Suppress any startup message in the echo area
(run-with-idle-timer 0.05 nil (lambda() (message nil)))

(me-splash-screen)

;; Close it automatically 3s after Emacs gets loaded
(add-hook
 'emacs-startup-hook
 (lambda ()
   (run-at-time 3 nil #'me-splash-screen-kill)))


(provide 'me-splash)
