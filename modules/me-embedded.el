;;; me-embedded.el --- Embedded systems stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2022-11-07
;; Last modified: 2025-09-15

;;; Commentary:

;;; Code:

;; Emacs package with utilities for embedded development with OpenOCD
(use-package embed
  :straight (:host github :repo "xal-0/embed-el"))


;; A set of Emacs modes for various Yocto/Bitbake file formats
(use-package bitbake
  :straight (bitbake-modes :host bitbucket :repo "olanilsson/bitbake-modes")
  :hook (bitbake-mode . bitbake-electric-mode)
  :config
  (require 'bitbake-insert)
  (require 'bitbake-electric))


;; A `treesit'-based Bitbake major mode
(use-package bitbake-ts-mode
  :straight t
  :disabled ; TEMP: No good syntax highlighting
  :when (featurep 'feat/tree-sitter)
  :config
  (add-to-list
   'treesit-language-source-alist
   '(bitbake "https://github.com/tree-sitter-grammars/tree-sitter-bitbake"))
  (treesit-ensure-installed 'bitbake))


(provide 'me-embedded)

;;; me-embedded.el ends here
