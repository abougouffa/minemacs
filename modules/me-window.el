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


(defconst +me-help-buffers-re
  (rx bol "*" (or "info" "Printing Help" "Org Entity Help" "General Keybindings" "tldr" "Dictionary" "lexic" "Completions"
                  (seq (or "Help" "helpful" "eldoc" "Tcl help" "Man " "WoMan " "eglot-help for " "shellcheck:" "show-marks") (* any)))
      "*" eol))

(defconst +me-warning-buffers-re
  (rx bol "*" (or "Warnings" "envrc") "*" eol))

(defconst +me-terminal-buffers-re
  (rx bol "*" (or "eshell" "terminal" "shell" "Shell Command Output" "Async Shell Command" (seq "vterminal - " (* any))) "*" eol))

(defconst +me-repl-buffers-re
  (rx bol "*"
      (or "scheme" "ielm" "Python" "Inferior Octave" "maxima" "imaxima" "lua"
          "inferior-lisp" "prolog" "gnuplot" "Nix-REPL" "julia"
          (seq (or (seq "R" (opt ":" (any digit))) "julia" "SQL") ":" (* any)))
      "*" eol))

;; Help/info buffers
(add-to-list
 'display-buffer-alist
 `(,+me-help-buffers-re
   (display-buffer-in-side-window)
   (slot . 0)
   (side . right)
   (dedicated . t) ;; Close when finished
   (window-width . 85)))

;; Show *Warnings* at bottom
(add-to-list
 'display-buffer-alist
 `(,+me-warning-buffers-re
   (display-buffer-reuse-window display-buffer-in-direction)
   (direction . bottom) ;; bottom (above below...)
   (dedicated . t) ;; Close when finished
   (reusable-frames . visible)
   (window-height . 10)))

;; Terminal buffers
(add-to-list
 'display-buffer-alist
 `(,+me-terminal-buffers-re
   (display-buffer-reuse-window display-buffer-at-bottom)
   (dedicated . t) ;; Close when finished
   (direction . bottom)
   (reusable-frames . visible) ;;
   (window-height . 0.2)))

;; REPL buffers
(add-to-list
 'display-buffer-alist
 `(,+me-repl-buffers-re
   (display-buffer-in-side-window)
   (side . right)
   (dedicated . t) ;; Close when finished
   (window-width . 0.5)
   (reusable-frames . visible)))


(setq frame-title-format '("GNU Emacs (%b)"))


(provide 'me-window)

;;; me-window.el ends here
