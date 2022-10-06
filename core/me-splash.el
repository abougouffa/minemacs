;; -*- lexical-binding: t; -*-


(require 'cl-lib)

(setq inhibit-splash-screen nil)

(defun me-splash-screen ()
  "Emacs splash screen"
  (interactive)
  (let* ((splash-buffer  (get-buffer-create "*minemacs-splash*"))
         (recover-session (and auto-save-list-file-prefix
                               (file-directory-p (file-name-directory
                                                  auto-save-list-file-prefix))))
         (height         (- (window-body-height nil) 1))
         (width          (window-body-width nil))
         (padding-center (- (/ height 2) 1))
         (padding-bottom (- height (/ height 2) 3)))

    ;; If there are buffer associated with filenames,
    ;;  we don't show splash screen.
    (if (eq 0 (length (cl-loop for buf in (buffer-list)
                               if (buffer-file-name buf)
                               collect (buffer-file-name buf))))

        (with-current-buffer splash-buffer
          (erase-buffer)

          ;; Buffer local settings
          (if (one-window-p)
              (setq mode-line-format nil))
          (setq cursor-type nil)
          (setq vertical-scroll-bar nil)
          (setq horizontal-scroll-bar nil)
          (setq fill-column width)
          (face-remap-add-relative 'link :underline nil)

          ;; Vertical padding to center
          (insert-char ?\n padding-center)

          ;; Central text
          (insert (concat (propertize "MinEmacs " 'face 'bold)
                          (format "(GNU Emacs %s)" emacs-version)))
          (center-line) (insert "\n")
          (insert-text-button " github.com/abougouffa/minemacs "
                              'action (lambda (_) (browse-url "https://github.com/abougouffa/minemacs"))
                              'help-echo "Visit MinEmacs repo"
                              'follow-link t)
          (center-line) (insert "\n")
          (insert (concat
                   "Loaded in "
                   (propertize (emacs-init-time) 'face 'italic)))
          (center-line) (insert "\n")
          (insert (propertize "A free/libre editor" 'face 'shadow))
          (center-line)


          ;; Vertical padding to bottom
          (insert-char ?\n padding-bottom)

          ;; Recover session button
          (when recover-session
            (delete-char -2)
            (insert-text-button " [Recover session] "
                                'action (lambda (_) (call-interactively 'recover-session))
                                'help-echo "Recover previous session"
                                'face 'warning
                                'follow-link t)
            (center-line) (insert "\n") (insert "\n"))

          ;; Copyright text
          (insert (propertize
                   "GNU Emacs comes with ABSOLUTELY NO WARRANTY" 'face 'shadow))
          (center-line) (insert "\n")
          (insert (propertize
                   "Copyright (C) 2022 Free Software Foundation, Inc." 'face 'shadow))
          (center-line) (insert "\n")

          (goto-char 0)
          (read-only-mode t)

          (local-set-key (kbd "C-[")       'splash-screen-kill)
          (local-set-key (kbd "<escape>")  'splash-screen-kill)
          (local-set-key (kbd "q")         'splash-screen-kill)
          (local-set-key (kbd "q")         'splash-screen-kill)
          (local-set-key (kbd "<mouse-1>") 'mouse-set-point)
          (local-set-key (kbd "<mouse-2>") 'operate-this-button)
          (display-buffer-same-window splash-buffer nil)
          (evil-insert-state)))))

(defun splash-screen-kill ()
  "Kill the splash screen buffer (immediately)."
  (interactive)
  (when (get-buffer "*minemacs-splash*")
    (kill-buffer "*minemacs-splash*")))

;; Suppress any startup message in the echo area
(run-with-idle-timer 0.05 nil (lambda() (message nil)))

;; Install hook after frame parameters have been applied and only if
;; no option on the command line
(if (and (not (member "-no-splash"  command-line-args))
         (not (member "--file"      command-line-args))
         (not (member "--insert"    command-line-args))
         (not (member "--find-file" command-line-args))
         (not inhibit-startup-screen))
    (progn
      (add-hook 'window-setup-hook 'me-splash-screen)
      (setq inhibit-startup-screen t
            inhibit-startup-message t
            inhibit-startup-echo-area-message t)))

(provide 'me-splash)
