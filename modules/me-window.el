;; -*- lexical-binding: t; -*-


(use-package popwin
  :straight (:host github :repo "emacsorphanage/popwin" :files (:defaults "*"))
  :after minemacs-loaded
  :config
  (defun +popwin-register (pred &rest args)
    (dolist (p (ensure-list pred))
      (push (cons p args) popwin:special-display-config)))

  (+popwin-register '("*Warnings*" compilation-mode) :height 8 :noselect t)
  (popwin-mode 1))


(when nil
  ;; Window configuration for special windows.
  ;; This section inspired by the article "Demystifying Emacs’s Window
  ;; Manager" found here:
  ;; https://www.masteringemacs.org/article/demystifying-emacs-window-manager
  (add-to-list
   'display-buffer-alist
   '("\\*Help\\*"
     (display-buffer-reuse-window display-buffer-pop-up-window)
     (inhibit-same-window . t)))

  (add-to-list
   'display-buffer-alist
   '("\\*Completions\\*"
     (display-buffer-reuse-window display-buffer-pop-up-window)
     (inhibit-same-window . t)
     (window-height . 10)))


  (add-to-list
   'display-buffer-alist
   '("\\*Warnings\\*"
     (display-buffer-reuse-window display-buffer-pop-up-window)
     (inhibit-same-window . t)
     (window-height . 10)))

  ;; Show dictionary definition on the left
  (add-to-list
   'display-buffer-alist
   '("^\\*Dictionary\\*"
     (display-buffer-in-side-window)
     (side . left)
     (window-width . 70))))


(setq frame-title-format
      '(""
        (:eval
         (if (and
              (boundp 'org-roam-directory)
              (string-prefix-p
               (expand-file-name org-roam-directory)
               (expand-file-name (or buffer-file-name ""))))
             (replace-regexp-in-string ".*/[0-9]*-?" "☰ "
              (subst-char-in-string ?_ ?\s buffer-file-name))
           "%b"))
        (:eval
         (let* ((proj (project-current))
                (proj (if proj
                          (project-root proj)
                        (ignore-errors
                          (file-name-nondirectory
                           (string-trim-right (expand-file-name (vc-root-dir)) "/"))))))
          (concat
           (format " in %s mode" (car (ensure-list mode-name)))
           (if (buffer-modified-p) " ○" " ●")
           (when proj (format " %s" (abbreviate-file-name proj))))))))


(provide 'me-window)
