;;; me-copilot.el --- Unofficial GitHub Copilot integration -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn-rkg@fntrzpbz.pbz")

;;; Commentary:

;;; Code:

;; Copilot integration
(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el")
  :bind (:map copilot-completion-map ([tab] . copilot-accept-completion))
  :custom
  (copilot-install-dir (expand-file-name "copilot" minemacs-local-dir)))




(provide 'obsolete/me-copilot)
;;; me-copilot.el ends here
