;;; me-edraw.el --- Emacs Draw, make simple SVG drawings inside Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-06-07
;; Last modified: 2025-04-30

;;; Commentary:

;;; Code:

(defconst +easydraw-available-p (and (featurep 'feat/rsvg) (featurep 'feat/zlib) (featurep 'feat/libxml2)))

(use-package edraw
  :straight (:host github :repo "misohena/el-easydraw")
  :when +easydraw-available-p)

(use-package edraw-org
  :hook (org-mode . edraw-org-setup-default)
  :when +easydraw-available-p)


(provide 'obsolete/me-edraw)
;;; me-edraw.el ends here
