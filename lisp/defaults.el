;; -*- lexical-binding: t; -*-

(set-default-coding-systems 'utf-8)

(setq visible-bell nil ;; set to non-nil to flash!
      ring-bell-function 'ignore
      large-file-warning-threshold 52428800 ;; change to 50 MiB
      use-short-answers t ;; y or n istead of yes or no
      confirm-kill-emacs 'yes-or-no-p ;; confirm before quitting
      initial-scratch-message ";; Scratch"
      frame-resize-pixelwise t
      source-directory (expand-file-name "~/Softwares/src/emacs/")
      trash-directory nil ;; Use FreeDesktop.org trashcan (default)
      delete-by-moving-to-trash t)


;; Try to keep the cursor from getting stuck in the read-only prompt part of the minibuffer.
;;(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
;;(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(provide 'minemacs-defaults)
