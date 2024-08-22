;;; me-scala.el --- Scala language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-scala
  :auto-mode '(("\\.\\(scala\\|sbt\\|worksheet\\.sc\\)\\'" . scala-mode))
  :companion-packages '(((scala-mode scala-ts-mode) . sbt-mode)))

(use-package scala-mode
  :straight t)

(use-package sbt-mode
  :straight t)


(provide 'on-demand/me-scala)
;;; me-scala.el ends here
