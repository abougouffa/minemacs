;;; me-page-break-lines.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-08-25
;; Last modified: 2025-08-25

;;; Commentary:

;;; Code:


;; Display "^L" page breaks as tidy horizontal lines
(use-package page-break-lines
  :straight t
  :hook ((prog-mode text-mode special-mode) . page-break-lines-mode))


(provide 'obsolete/me-page-break-lines)
;;; me-page-break-lines.el ends here
