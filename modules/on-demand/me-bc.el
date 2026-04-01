;;; me-bc.el --- Basic Calculator -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn@cneebg.pbz")
;; Created: 2026-03-31
;; Last modified: 2026-03-31

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-bc
  :auto-mode '((".bc\\'" . bc-mode))
  :interpreter-mode '(("bc" . bc-mode)))


;; BC code editing commands for Emacs
(use-package bc-mode
  :straight t
  :mode "\\.bc\\'"
  :interpreter "bc")


(provide 'on-demand/me-bc)
;;; me-bc.el ends here
