;;; me-window.el --- Windows and frames -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;; Window configuration for special windows.
;; This section inspired by the article "Demystifying Emacs’s Window Manager" found here:
;; www.masteringemacs.org/article/demystifying-emacs-window-manager

;;; Code:

(use-package ace-window
  :straight t
  :bind (("M-o" . ace-window))
  :custom
  (aw-dispatch-always t))

(use-package window-purpose
  :straight t)

;; Help/info buffers
(add-to-list
 'display-buffer-alist
 `(,(rx bol "*" (or "info" "Printing Help" "Org Entity Help" "General Keybindings" "tldr" (seq (or "Help" "helpful" "eldoc" "Tcl help" "Man " "WoMan " "eglot-help for ") (* any))) "*")
   (display-buffer-in-side-window)
   (slot . 0)
   (side . right)
   (dedicated . t) ;; Close when finished
   (window-width . 85)))

;; Show *Warnings* at bottom
(add-to-list
 'display-buffer-alist
 `("^\\*Warnings\\*$"
   (display-buffer-reuse-window display-buffer-in-direction)
   (direction . bottom) ;; bottom (above below...)
   (dedicated . t) ;; Close when finished
   (reusable-frames . visible) ;;
   (window-height . 10)))

;; Show dictionary definition and completion buffer on the right side
(add-to-list
 'display-buffer-alist
 `(,(rx bol "*" (or "Dictionary" "lexic" "Completions") "*" eol)
   (display-buffer-in-side-window)
   (side . right)
   (window-width . 85)))

;; Terminal buffers
(add-to-list
 'display-buffer-alist
 `(,(rx bol "*" (or "eshell" "terminal" "shell" "Shell Command Output" "Async Shell Command" (seq "vterminal - " (* any))) "*" eol)
   (display-buffer-reuse-window display-buffer-at-bottom)
   (dedicated . t) ;; Close when finished
   (direction . bottom)
   (reusable-frames . visible) ;;
   (window-height . 0.3)))

;; REPL buffers
(add-to-list
 'display-buffer-alist
 `(,(rx bol "*"
     (or "scheme" "ielm" "Python" "Inferior Octave" "maxima" "imaxima" "lua"
      "inferior-lisp" "prolog" "gnuplot" "Nix-REPL" "julia"
      (seq (or (seq "R" (opt ":" (any digit))) "julia" "SQL") ":" (* any)))
     "*" eol)
   (display-buffer-in-side-window)
   (side . right)
   (dedicated . t) ;; Close when finished
   (window-width . 0.5)
   (reusable-frames . visible)))

(setq frame-title-format
      '("GNU Emacs (" "%b"
        (:eval
         (let ((proj (cond
                      ((featurep 'projectile) (projectile-project-name))
                      (t (or
                          (ignore-errors (project-name (project-current)))
                          (ignore-errors (file-name-nondirectory (string-trim-right (expand-file-name (vc-root-dir)) "/")))
                          "*no-project*")))))
          (concat
           (if (buffer-modified-p) " ○" " ●")
           (when (and proj (not (string= proj "-"))) (format " %s" proj)))))
        ")"))

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

  (+map! "wj" '(+window-adjust-size/body :wk "+window-adjust-size")))


(provide 'me-window)

;;; me-window.el ends here
