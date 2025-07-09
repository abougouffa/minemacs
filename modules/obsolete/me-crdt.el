;;; me-crdt.el --- CRDT -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-07-09
;; Last modified: 2025-07-09

;;; Commentary:

;;; Code:


;; Collaborative editing using Conflict-free Replicated Data Types
(use-package crdt
  :straight t
  :when (or (executable-find "tuntox") (executable-find "stunnel"))
  :custom
  (crdt-tuntox-password-in-url t)
  (crdt-use-tuntox (executable-find "tuntox"))
  (crdt-use-stunnel (executable-find "stunnel")))


(provide 'obsolete/me-crdt)
;;; me-crdt.el ends here
