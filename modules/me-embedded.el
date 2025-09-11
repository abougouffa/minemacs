;;; me-embedded.el --- Embedded systems stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2022-11-07
;; Last modified: 2025-09-11

;;; Commentary:

;;; Code:

;; Emacs package with utilities for embedded development with OpenOCD
(use-package embed
  :straight (:host github :repo "xal-0/embed-el"))


;; A set of Emacs modes for various Yocto/Bitbake file formats
(use-package bitbake
  :straight (bitbake-modes :host bitbucket :repo "olanilsson/bitbake-modes")
  :hook (bitbake-mode . bitbake-electric-mode)
  :autoload (+bitbake-poky-sources)
  :commands (+bitbake-insert-poky-sources)
  :config
  (require 'bitbake-insert)
  (require 'bitbake-electric)
  :init
  (defun +widget-choose-completion (prompt items &optional _event)
    "Same interface as `widget-choose' but uses `completing-read' under the hood."
    (let ((choice (completing-read (format "%s: " prompt) (mapcar #'car items))))
      (alist-get choice items nil nil #'equal)))

  ;; `bitbake' uses `widget-choose' to choose, but I prefer `completing-read', so lets overwrite it!
  (satch-advice-add
   '(bitbake-recipe-build-dir bitbake-recipe-build-dir-dired) :around
   (satch-defun +widget-choose--use-completion-read (fn &rest args)
     (cl-letf (((symbol-function 'widget-choose) #'+widget-choose-completion))
       (apply fn args)))))


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
