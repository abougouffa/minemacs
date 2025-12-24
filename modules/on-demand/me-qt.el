;;; me-qt.el --- Qt support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-11
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-qt
  :auto-mode '(("\\.qml\\'" . qml-mode) ("\\.qml\\'" . qml-ts-mode) ("\\.pr[io]\\'" . qt-pro-mode)))

;;;###autoload(add-to-list 'auto-mode-alist '("\\.qss\\'" . css-mode))
;;;###autoload(add-to-list 'auto-mode-alist '("\\.qrc\\'" . xml-mode))


;; Major mode for editing QT Declarative (QML) code
(use-package qml-mode
  :ensure t)


;; Tree-sitter based major mode for editing QT Declarative (QML) code
(use-package qml-ts-mode
  :vc (:url "https://github.com/xhcoding/qml-ts-mode")
  :when (featurep 'feat/tree-sitter)
  :config
  (add-to-list
   'treesit-language-source-alist
   '(qmljs "https://github.com/yuja/tree-sitter-qmljs"))
  (treesit-ensure-installed 'dart))


;; Major mode for Qt's Pro/Pri files
(use-package qt-pro-mode
  :ensure t
  :mode "\\.pr[io]\\'")


(provide 'on-demand/me-qt)
;;; me-qt.el ends here
