;;; me-experimental.el --- Experimental packages -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-12-11
;; Last modified: 2025-06-18

;;; Commentary:

;;; Code:


;; Extra non-standard functionalities for Eglot
(use-package eglot-x
  :straight (:host github :repo "nemethf/eglot-x")
  :commands (eglot-x-setup))


;; A Dynamic Module for WebKit, aka a fully fledged browser inside Emacs
(use-package webkit
  :straight `( :host github :repo "akirakyle/emacs-webkit"
               :files (:defaults "*.js" "*.css" "*.so")
               :pre-build ,(if (featurep 'os/win)
                               '(message "The `webkit' module won't build on Windows.")
                             '("make")))
  :when (and (featurep 'feat/modules) (not (featurep 'os/win))))


;; A Dynamic Module for WebKit, aka a fully fledged browser inside Emacs
(use-package webkit-dark
  :straight webkit
  :when (and (featurep 'feat/modules) (not (featurep 'os/win)))
  :bind (:map webkit-mode-map ("C-c d" . webkit-dark-toggle)))


;; Offline documentation browser using Dash/Zeal docsets
(use-package dash-docs
  :straight (:host github :repo "abougouffa/dash-docs")
  :custom
  (dash-docs-docsets-path (concat minemacs-local-dir "docsets/"))
  (dash-docs-browser-func #'eww-browse-url)
  :init
  (+setq-hook! (c-mode c-ts-mode) dash-docs-docsets '("C" "OpenCV" "OpenCV C"))
  (+setq-hook! (c++-mode c++-ts-mode) dash-docs-docsets '("C++" "OpenCV" "OpenCV C++")))


;; Integration of `consult' with `dash-docs'
(use-package consult-dash
  :straight t)


(provide 'me-experimental)
;;; me-experimental.el ends here
