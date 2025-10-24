;;; me-eglot-booster.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn@cneebg.pbz")
;; Created: 2025-10-24
;; Last modified: 2025-10-24

;;; Commentary:

;;; Code:


;; Boost `eglot' using `emacs-lsp-booster' (github.com/blahgeek/emacs-lsp-booster)
(use-package eglot-booster
  :straight (:host github :repo "jdtsmith/eglot-booster")
  :custom
  ;; Emacs' 30 JSON parser is much faster, no need to byte-encode, see:
  ;; https://github.com/jdtsmith/eglot-booster?tab=readme-ov-file#io-only
  (eglot-booster-io-only (>= emacs-major-version 30)))


(provide 'obsolete/me-eglot-booster)
;;; me-eglot-booster.el ends here
