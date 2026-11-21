;;; me-copilot.el --- Unofficial GitHub Copilot integration -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn-rkg@fntrzpbz.pbz")
;; Created: 2025-02-04
;; Last modified: 2026-11-21

;;; Commentary:

;;; Code:


;; Copilot integration
(use-package copilot
  :ensure t
  :bind (:map copilot-completion-map ([tab] . copilot-accept-completion))
  :custom
  (copilot-install-dir (expand-file-name "copilot" minemacs-local-dir)))


(provide 'obsolete/me-copilot)
;;; me-copilot.el ends here
