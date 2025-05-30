;;; me-pdf.el --- PDF documents -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-16
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-pdf
  :auto-mode '(("\\.[pP][dD][fF]\\'" . pdf-view-mode))
  :magic-mode '(("%PDF" . pdf-view-mode))
  :companion-packages '((doc-view-mode . (pdf-view-mode pdf-isearch-minor-mode pdf-view-restore-mode))))


;; View and annotate PDF files
(use-package pdf-tools
  :straight t
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :hook (minemacs-build-functions . pdf-tools-install)
  :custom
  (pdf-view-display-size 'fit-page)
  (pdf-view-image-relief 2)
  (pdf-view-use-scaling t))


;; Isearch support in PDF buffers
(use-package pdf-isearch
  :hook (pdf-view-mode . pdf-isearch-minor-mode))


;; Support for opening last known pdf position in `pdf-view-mode'
(use-package pdf-view-restore
  :straight t
  :hook (pdf-view-mode . pdf-view-restore-mode)
  :custom
  (use-file-base-name-flag nil)
  (pdf-view-restore-filename (concat minemacs-local-dir "pdf-view-restore.el")))


(provide 'on-demand/me-pdf)
;;; me-pdf.el ends here
