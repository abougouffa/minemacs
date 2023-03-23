;;; me-window.el --- Windows and frames -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")


;; Window configuration for special windows.
;; This section inspired by the article "Demystifying Emacs’s Window Manager" found here:
;; www.masteringemacs.org/article/demystifying-emacs-window-manager
(+deferred!
 (add-to-list
  'display-buffer-alist
  `(,(rx (seq "*" (or "Help" (seq "helpful" (zero-or-more not-newline))) "*"))
    (display-buffer-reuse-window display-buffer-pop-up-window)
    (inhibit-same-window . t)))

 ;; Show *Warnings* at bottom
 (add-to-list
  'display-buffer-alist
  `("*Warnings*"
    (display-buffer-reuse-window display-buffer-in-direction)
    (direction . bottom) ;; bottom (above below...)
    (dedicated . t) ;; Close when finished
    (reusable-frames . visible) ;;
    (window-height . 10)))

 ;; Show dictionary definition and completion buffer on the right side
 (add-to-list
  'display-buffer-alist
  `(,(rx (seq "*" (or "Dictionary" "lexic" "Completions") "*"))
    (display-buffer-in-side-window)
    (side . right)
    (window-width . 82)))

 (add-to-list
  'display-buffer-alist
  `(,(rx (seq "*" (or "eshell" "vterm" "terminal") "*"))
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
            (if (buffer-modified-p) " ○" " ●")
            (when (and proj (not (string= proj "-")))
             (format " %s" proj)))))))

 ;; Adapted from: github.com/Phundrak/dotfiles/blob/master/org/config/emacs.org
 (with-eval-after-load 'hydra
   (defhydra +window-adjust-size (:hint nil :foreign-keys warn)
     "
^Zoom^                                ^Other
^^^^^^^-----------------------------------------
[_t_/_s_] shrink/enlarge vertically   [_q_] quit
[_c_/_r_] shrink/enlarge horizontally
"
     ("q" nil :exit t)
     ("c" shrink-window-horizontally)
     ("t" enlarge-window)
     ("s" shrink-window)
     ("r" enlarge-window-horizontally))

   (+map!
     :infix "w"
     "a" '(+window-adjust-size/body :wk "Adjust window size"))))


(provide 'me-window)
