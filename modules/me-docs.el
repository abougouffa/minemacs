;;; me-docs.el --- Documents (PDF, EPUB, DOC...) -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package pdf-tools
  :straight t
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :hook (minemacs-build-functions . pdf-tools-install)
  :custom
  (pdf-view-display-size 'fit-page)
  (pdf-view-image-relief 2)
  (pdf-view-use-scaling t))

(use-package pdf-isearch
  :hook (pdf-view-mode . pdf-isearch-minor-mode))

(use-package pdf-view-restore
  :straight t
  :hook (pdf-view-mode . pdf-view-restore-mode)
  :custom
  (use-file-base-name-flag nil)
  (pdf-view-restore-filename (concat minemacs-local-dir "pdf-view-restore.el")))

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

(use-package pandoc-mode
  :straight t
  :hook (markdown-mode . conditionally-turn-on-pandoc))

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
