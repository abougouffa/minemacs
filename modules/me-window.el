;;; me-window.el --- Windows and frames -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


;; Window configuration for special windows.
;; This section inspired by the article "Demystifying Emacs’s Window Manager" found here:
;; https://www.masteringemacs.org/article/demystifying-emacs-window-manager
(add-to-list
 'display-buffer-alist
 `(,(rx (seq "*" (or "Help" (seq "helpful" (zero-or-more not-newline))) "*"))
   (display-buffer-reuse-window display-buffer-pop-up-window)
   (inhibit-same-window . t)))

(add-to-list
 'display-buffer-alist
 `(,(rx (seq "*" (or "Completions" "Warnings") "*"))
   (display-buffer-reuse-window display-buffer-pop-up-window)
   (inhibit-same-window . t)
   (window-height . 10)))

;; Show dictionary definition on the left
(add-to-list
 'display-buffer-alist
 `(,(rx (seq "*" (or "Dictionary" "lexic") "*"))
   (display-buffer-in-side-window)
   (side . right)
   (window-width . 70)))

(add-to-list
 'display-buffer-alist
 '((lambda (buffer-or-name _)
     (let ((buffer (get-buffer buffer-or-name)))
      (with-current-buffer buffer
       (or (memq major-mode '(vterm-mode eshell-mode))
        (string-prefix-p vterm-buffer-name (buffer-name buffer))
        (string-prefix-p eshell-buffer-name (buffer-name buffer))))))
   ;; (display-buffer-reuse-window display-buffer-at-bottom)
   (display-buffer-reuse-window display-buffer-in-direction)
   (direction . bottom) ;; bottom (above below...)
   (dedicated . t) ;; Close when finished
   (reusable-frames . visible) ;;
   (window-height . 0.3)))

(setq frame-title-format
      '(""
        (:eval
         (if (and
              (bound-and-true-p org-roam-directory)
              (string-prefix-p
               (expand-file-name org-roam-directory)
               (expand-file-name (or buffer-file-name ""))))
             (replace-regexp-in-string ".*/[0-9]*-?" "☰ "
              (subst-char-in-string ?_ ?\s buffer-file-name))
           "%b"))
        (:eval
         (let ((proj
                (ignore-errors
                  (cond
                   ((featurep 'projectile)
                    (projectile-project-name))
                   (t
                    (or
                     (project-name (project-current))
                     (file-name-nondirectory
                      (string-trim-right (expand-file-name (vc-root-dir)) "/"))))))))
          (concat
           (format " {%s}" (symbol-name major-mode))
           (if (buffer-modified-p) " ○" " ●")
           (when (and proj (not (string= proj "-")))
            (format " %s" proj)))))))


(provide 'me-window)
