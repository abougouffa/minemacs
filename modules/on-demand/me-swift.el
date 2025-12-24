;;; me-swift.el --- Swift language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-23
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-swift
  :auto-mode '(("\\.swift\\(interface\\)?\\'" . swift-mode)))


;; Major-mode for Apple's Swift programming language
(use-package swift-mode
  :ensure t)


;; Major mode for Swift based on Tree-sitter
(use-package swift-ts-mode
  :ensure t
  :when (featurep 'feat/tree-sitter)
  :config
  (add-to-list 'treesit-language-source-alist '(swift "https://github.com/alex-pinkus/tree-sitter-swift"))
  (treesit-ensure-installed 'swift))


(provide 'on-demand/me-swift)
;;; me-swift.el ends here
