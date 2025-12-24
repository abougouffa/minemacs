;;; me-envrc.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn@cneebg.pbz")
;; Created: 2025-10-23
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:


;; Buffer-local "direnv" integration for Emacs
(use-package envrc
  :ensure t
  :hook (minemacs-first-file . envrc-global-mode)
  :when (and (not (featurep 'os/win)) (executable-find "direnv"))
  :custom
  (envrc-debug minemacs-debug-p)
  (envrc-remote t)
  (envrc-supported-tramp-methods '("ssh" "docker"))
  :config
  (keymap-set minemacs-open-thing-map "v" envrc-command-map)
  (with-eval-after-load 'ob ; Ensure loading envrc for babel source blocks
    (advice-add #'org-babel-execute-src-block :around #'envrc-propagate-environment)))


(provide 'obsolete/me-envrc)
;;; me-envrc.el ends here
