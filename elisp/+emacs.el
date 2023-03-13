;; -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")


;;;###autoload
(defun +dir-locals-reload-for-this-buffer ()
  "Reload directory-local for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)
    (+info! "Reloaded directory-local variables for buffer %s"
            (buffer-name (current-buffer)))))

;;;###autoload
(defun +dir-locals-reload-for-all-buffers-in-this-directory ()
  "For every buffer with the same `default-directory` as the
current buffer's, reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir)
          (+dir-locals-reload-for-this-buffer))))))

(defun +dir-locals--autoreload-h ()
  (when (and (buffer-file-name)
             (equal dir-locals-file (file-name-nondirectory (buffer-file-name))))
    (+dir-locals-reload-for-all-buffers-in-this-directory)
    (message "Reloaded directory-local variables defined in %s." dir-locals-file)))

(defvar +dir-locals--autoreload-p nil)

;;;###autoload
(defun +dir-locals-toggle-autoreload (&optional enable)
  "Toggle autoloading directory-local variables after editing the \".dir-locals\" file.
If ENABLE is non-nil, force enabling autoreloading."
  (interactive)
  (if (or enable +dir-locals--autoreload-p)
      (progn
        (remove-hook 'after-save-hook #'+dir-locals--autoreload-h)
        (setq +dir-locals--autoreload-p nil)
        (message "Disabled auto-reloading directory-locals."))
    (add-hook 'after-save-hook #'+dir-locals--autoreload-h)
    (setq +dir-locals--autoreload-p t)
    (message "Enabled auto-reloading directory-locals.")))

;;;###autoload
(defun +dir-locals-open-or-create ()
  "Open or create the dir-locals.el for the current project."
  (interactive)
  (let* ((file-name (buffer-file-name))
         (base-dir (car (ensure-list (dir-locals-find-file file-name)))))
    (find-file
     (cond (base-dir (expand-file-name dir-locals-file base-dir))
           ((project-current) (expand-file-name dir-locals-file (project-root (project-current))))
           ((vc-root-dir) (expand-file-name dir-locals-file (vc-root-dir)))
           (t (expand-file-name dir-locals-file (file-name-directory file-name)))))))

;; The hook is defined and enabled by default in `me-defaults'
;;;###autoload
(defun +toggle-auto-whitespace-cleanup ()
  "Toggle auto-deleting trailing whitespaces."
  (interactive)
  (if (member #'+save--whitespace-cleanup-h before-save-hook)
      (progn
        (message "+toggle-auto-whitespace-cleanup: Disabled.")
        (remove-hook 'before-save-hook #'+save--whitespace-cleanup-h))
    (message "+toggle-auto-whitespace-cleanup: Enabled.")
    (add-hook 'before-save-hook #'+save--whitespace-cleanup-h)))

;; Adapted from: rougier/nano-emacs
;;;###autoload
(defun +what-faces (pos)
  "Get the font faces at POS."
  (interactive "d")
  (let ((faces (remq nil
                     (list
                      (get-char-property pos 'read-face-name)
                      (get-char-property pos 'face)
                      (plist-get (text-properties-at pos) 'face)))))
    (message "Faces: %s" faces)))

(defcustom +screenshot-delay 5
  "A delay to wait before taking the screenshot.
Applicable only when calling `+screenshot-svg' with a prefix.")

;; Inspired by: reddit.com/r/emacs/comments/idz35e/comment/g2c2c6y
;;;###autoload
(defun +screenshot-svg ()
  "Save a screenshot of the current frame as an SVG image.
Saves to a temp file and puts the filename in the kill ring. If launched with a
prefix or universal argument, it waits for a moment (defined by
`+screenshot-delay') before taking the screenshot."
  (interactive)
  (if current-prefix-arg
      (run-with-timer +screenshot-delay nil #'+screenshot-svg--take-screenshot)
    (+screenshot-svg--take-screenshot)))

(defun +screenshot-svg--take-screenshot ()
  (let* ((filename (make-temp-file "emacs-" nil ".svg"))
         (data (x-export-frames nil 'svg)))
    (with-temp-file filename (insert data))
    (kill-new filename)
    (message "Screenshot saved to %s" filename)))
