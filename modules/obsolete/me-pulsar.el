;;; me-pulsar.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-11-16
;; Last modified: 2025-11-16

;;; Commentary:

;;; Code:


;; Pulse highlight on demand or after select functions
(use-package pulsar
  :straight (:host github :repo "protesilaos/pulsar")
  :hook (minemacs-first-file . pulsar-global-mode)
  :custom
  (pulsar-pulse-on-window-change t)
  (pulsar-region-face 'pulsar-green)
  (pulsar-highlight-face 'pulsar-cyan)
  (pulsar-region-change-face 'pulsar-red)
  (pulsar-window-change-face 'pulsar-yellow)
  :config
  (cl-callf append pulsar-pulse-functions '(what-cursor-position)))


(provide 'obsolete/me-pulsar)
;;; me-pulsar.el ends here
