;;; me-tree-sitter.el --- Tree-sitter integraion for Emacs28 and earlier -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package tree-sitter
  :ensure t
  :defer 5
  :when (+emacs-options-p 'arch/x86_64)
  :preface
  (advice-add 'tsc-dyn-get-ensure :around '+apply-inhibit-messages)
  :hook (tree-sitter-after-on . tree-sitter-hl-mode)
  :config
  (global-tree-sitter-mode 1))

(use-package tree-sitter-langs
  :ensure t
  :hook (minemacs-build-functions . tree-sitter-langs-install-grammars)
  :preface
  (advice-add 'tree-sitter-langs-install-grammars :around '+apply-inhibit-messages)
  :after tree-sitter
  :demand)

(use-package ts-fold
  :vc (:url "https://github.com/emacs-tree-sitter/ts-fold")
  :after tree-sitter
  :init
  (global-ts-fold-mode 1))


(provide 'obsolete/me-tree-sitter)
;;; me-tree-sitter.el ends here
