;;; me-swift.el --- Swift language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

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
  :when (+emacs-options-p 'tree-sitter))


(provide 'on-demand/me-swift)
;;; me-swift.el ends here
