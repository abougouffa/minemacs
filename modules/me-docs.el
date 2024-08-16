;;; me-docs.el --- Documents (PDF, EPUB, DOC...) -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package pdfgrep
  :straight t
  :commands (pdfgrep-mode pdfgrep)
  :custom
  (pdfgrep-options " -H -n -r --include \"*.pdf\" ")
  :config
  (pdfgrep-mode 1))

(defconst +tuntox-available-p (and (executable-find "tuntox") t))
(defconst +stunnel-available-p (and (executable-find "stunnel") t))

(use-package crdt
  :straight t
  :when (or +tuntox-available-p +stunnel-available-p)
  :custom
  (crdt-tuntox-password-in-url t)
  (crdt-use-tuntox +tuntox-available-p)
  (crdt-use-stunnel +stunnel-available-p))

(use-package rfc-mode
  :straight t
  :custom
  (rfc-mode-directory (concat minemacs-local-dir "rfc"))
  :init
  ;; Use a window wide enough but not too wide
  (add-to-list
   'display-buffer-alist
   `((derived-mode . rfc-mode)
     (display-buffer-in-side-window)
     (slot . 0)
     (side . right)
     (dedicated . t) ;; Close when finished
     (window-width . 76))))


(provide 'me-docs)

;;; me-docs.el ends here
