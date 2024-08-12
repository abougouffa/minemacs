;;; me-elm.el --- Elm language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn-rkg@fntrzpbz.pbz")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-elm
  :auto-mode '(("\\.elm\\'" . elm-mode))
  :companion-packages '((elm-mode . elm-test-runner)))

(use-package elm-mode
  :straight t)

(use-package elm-test-runner
  :straight t)


(provide 'on-demand/me-elm)
;;; me-elm.el ends here
