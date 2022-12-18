;; -*- lexical-binding: t; -*-

;;;###autoload
(defun +dir-locals-reload-for-this-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

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

;;;###autoload
(defun +dir-locals-enable-autoreload ()
  (when (and (buffer-file-name)
             (equal dir-locals-file (file-name-nondirectory (buffer-file-name))))
    (message "Dir-locals will be reloaded after saving.")
    (add-hook 'after-save-hook '+dir-locals-reload-for-all-buffers-in-this-directory nil t)))

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
(defun +toggle-auto-delete-trailing-whitespaces ()
  "Toggle auto-deleting trailing whitespaces."
  (interactive)
  (if (member #'+save--delete-trailing-whitespace-h before-save-hook)
      (remove-hook 'before-save-hook #'+save--delete-trailing-whitespace-h)
    (add-hook 'before-save-hook #'+save--delete-trailing-whitespace-h)))

;; From rougier/nano-emacs
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
