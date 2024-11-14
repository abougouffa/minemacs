;;; me-irony.el --- irony-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn-rkg@fntrzpbz.pbz")

;;; Commentary:

;;; Code:

;; A C/C++ minor mode for Emacs powered by "libclang"
(use-package irony-mode
  :straight t
  :config
  (when (+emacs-options-p 'os/win) ; Windows performance tweaks
    (when (boundp 'w32-pipe-read-delay) (setq w32-pipe-read-delay 0))
    ;; Set the buffer size to 64K on Windows (from the original 4K)
    (when (boundp 'w32-pipe-buffer-size) (setq irony-server-w32-pipe-buffer-size (* 64 1024)))))


;; Integration of `irony-mode' with `eldoc'
(use-package irony-eldoc
  :straight t)


(provide 'obsolete/me-irony)
;;; me-irony.el ends here
