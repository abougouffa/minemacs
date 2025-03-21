;;; me-edraw.el --- Emacs Draw, make simple SVG drawings inside Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(defconst +easydraw-available-p (+emacs-options-p 'rsvg 'zlib 'libxml2))

(use-package edraw
  :straight (:host github :repo "misohena/el-easydraw")
  :when +easydraw-available-p)

(use-package edraw-org
  :hook (org-mode . edraw-org-setup-default)
  :when +easydraw-available-p)


(provide 'obsolete/me-edraw)
;;; me-edraw.el ends here
