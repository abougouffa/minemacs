;;; me-wgrep.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn@cneebg.pbz")
;; Created: 2025-07-30
;; Last modified: 2025-07-30

;;; Commentary:

;;; Code:


;; Writable grep buffer and apply the changes to files
(use-package wgrep
  :straight t
  :when (< emacs-major-version 31) ; Emacs 31+ natively supports editing `grep-mode' buffers via `grep-edit-mode'
  :commands (wgrep-change-to-wgrep-mode)
  :custom
  (wgrep-auto-save-buffer t))


(provide 'obsolete/me-wgrep)
;;; me-wgrep.el ends here
