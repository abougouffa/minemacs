;;; me-webkit.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn@cneebg.pbz")
;; Created: 2025-07-08
;; Last modified: 2025-07-08

;;; Commentary:

;;; Code:


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


(provide 'obsolete/me-webkit)
;;; me-webkit.el ends here
