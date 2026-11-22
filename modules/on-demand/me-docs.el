;;; me-docs.el --- Documents -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2026-11-22
;; Last modified: 2026-11-22

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-docs
  :auto-mode `((,(rx "." (or "pdf" "epub" "mobi" "fb2" "xps" "cbz" "docx" "pptx" "xlsx" "odt" "ods" "odp" "odg") eol) . emureader-mode))
  :magic-mode '(("%PDF" . emureader-mode))
  :companion-packages '((doc-view-mode . emureader-mode)))


;; A muPDF based documents reader
(use-package emureader
  :ensure t
  :hook (minemacs-build-functions . emureader-build))


(provide 'on-demand/me-docs)
;;; me-docs.el ends here
