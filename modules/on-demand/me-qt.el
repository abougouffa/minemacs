;;; me-qt.el --- Qt support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Last modified: 2025-04-17

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-qt
  :auto-mode '(("\\.qml\\'" . qml-mode) ("\\.pr[io]\\'" . qt-pro-mode)))

;;;###autoload(add-to-list 'auto-mode-alist '("\\.qss\\'" . css-mode))
;;;###autoload(add-to-list 'auto-mode-alist '("\\.qrc\\'" . xml-mode))


;; Major mode for editing QT Declarative (QML) code
(use-package qml-mode
  :straight t)


;; Major mode for Qt's Pro/Pri files
(use-package qt-pro-mode
  :straight t
  :mode "\\.pr[io]\\'")


(provide 'on-demand/me-qt)
;;; me-qt.el ends here
