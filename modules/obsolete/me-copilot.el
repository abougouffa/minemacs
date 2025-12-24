;;; me-copilot.el --- Unofficial GitHub Copilot integration -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn-rkg@fntrzpbz.pbz")
;; Created: 2025-02-04
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:


;; Copilot integration
(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el")
  :bind (:map copilot-completion-map ([tab] . copilot-accept-completion))
  :custom
  (copilot-install-dir (expand-file-name "copilot" minemacs-local-dir)))


(provide 'obsolete/me-copilot)
;;; me-copilot.el ends here
