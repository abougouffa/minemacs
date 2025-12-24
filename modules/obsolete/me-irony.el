;;; me-irony.el --- irony-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn-rkg@fntrzpbz.pbz")
;; Created: 2024-11-06
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;; A C/C++ minor mode for Emacs powered by "libclang"
(use-package irony-mode
  :ensure t
  :config
  (when (featurep 'os/win) ; Windows performance tweaks
    (when (boundp 'w32-pipe-read-delay) (setq w32-pipe-read-delay 0))
    ;; Set the buffer size to 64K on Windows (from the original 4K)
    (when (boundp 'w32-pipe-buffer-size) (setq irony-server-w32-pipe-buffer-size (* 64 1024)))))


;; Integration of `irony-mode' with `eldoc'
(use-package irony-eldoc
  :ensure t)


(provide 'obsolete/me-irony)
;;; me-irony.el ends here
