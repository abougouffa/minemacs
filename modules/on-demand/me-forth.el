;;; me-forth.el --- Forth language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn-rkg@fntrzpbz.pbz")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-forth
  :auto-mode '(("\\.\\(fth\\|4th\\)\\'" . forth-mode)))

(use-package forth-mode
  :straight t)


(provide 'on-demand/me-forth)
;;; me-forth.el ends here
