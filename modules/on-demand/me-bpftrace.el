;;; me-bpftrace.el --- bpftrace script language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2026-10-06
;; Last modified: 2026-10-07

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-bpftrace
  :auto-mode '(("\\.bt\\'" . bpftrace-mode))
  :interpreter-mode '(("bpftrace" . bpftrace-mode)))


;; Major mode for bpftrace scripts
(use-package bpftrace-mode
  :straight (:host github :repo "abougouffa/bpftrace-mode"))


(provide 'on-demand/me-bpftrace)
;;; me-bpftrace.el ends here
