;; -*- lexical-binding: t; -*-


(use-package popwin
  :straight (:host github :repo "emacsorphanage/popwin" :files (:defaults "*"))
  :after minemacs-loaded
  :config
  (defun +popwin-register (pred &rest args)
    (if (listp pred)
        (dolist (p pred)
          (push (cons p args) popwin:special-display-config))
      (push (cons pred args) popwin:special-display-config)))

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


(provide 'me-window)
