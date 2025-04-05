;;; me-window.el --- Windows and frames -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;; Window configuration for special windows.
;; This section inspired by the article "Demystifying Emacs’s Window Manager" found here:
;; https://www.masteringemacs.org/article/demystifying-emacs-window-manager

;;; Code:

;; Quickly switch windows in Emacs
(use-package ace-window
  :straight t
  :bind (("M-o" . ace-window)))


;; Manage windows and buffers according to purposes
(use-package window-purpose
  :straight t)


;; Help/info buffers
(add-to-list
 'display-buffer-alist
 `(,(rx bol "*" (or "info" "Printing Help" "Org Entity Help" "General Keybindings" "tldr" (seq (or "Help" "helpful" "eldoc" "Tcl help" "Man " "WoMan " "eglot-help for " "shellcheck:") (* any))) "*")
   (display-buffer-in-side-window)
   (slot . 0)
   (side . right)
   (dedicated . t) ;; Close when finished
   (window-width . 85)))

;; Show *Warnings* at bottom
(add-to-list
 'display-buffer-alist
 `(,(rx bol "*" (or "Warnings" "envrc") "*" eol)
   (display-buffer-reuse-window display-buffer-in-direction)
   (direction . bottom) ;; bottom (above below...)
   (dedicated . t) ;; Close when finished
   (reusable-frames . visible) ;;
   (window-height . 10)))

;; Show dictionary definition and completion buffer on the right side
(add-to-list
 'display-buffer-alist
 `(,(rx bol "*" (or "Dictionary" "lexic" "Completions" (seq "show-marks" (* any))) "*" eol)
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
   (window-height . 0.2)))

(defvar +buffer-display-zoom-levels
  `((,(rx bol "*" (or "eshell" "terminal" "shell" "Shell Command Output" "Async Shell Command" (seq "vterminal - " (* any))) "*" eol)
     . -0.7)
    (,(rx bol "*" (or "Warnings" "envrc") "*" eol) . -0.7)))

(advice-add
 'display-buffer
 :after
 (satch-defun +display-buffer--change-font-size (buffer-or-name &optional action frame)
   (when-let* ((zoom-level (cdr (assoc (buffer-name) +buffer-display-zoom-levels #'buffer-match-p))))
     (if (< zoom-level 0)
         (text-scale-decrease (abs zoom-level))
       (text-scale-increase zoom-level)))))

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


(setq frame-title-format '("GNU Emacs (%b)"))


(provide 'me-window)

;;; me-window.el ends here
