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

(setq undo-limit        10000000 ;; 1MB   (default is 160kB)
      undo-strong-limit 100000000 ;; 100MB (default is 240kB)
      undo-outer-limit  1000000000) ;; 1GB   (default is 24MB)

(provide 'minemacs-defaults)
