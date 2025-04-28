;;; me-scala.el --- Scala language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-12
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-scala
  :auto-mode '(("\\.\\(scala\\|sbt\\|worksheet\\.sc\\)\\'" . scala-mode))
  :companion-packages '(((scala-mode scala-ts-mode) . sbt-mode)))


;; Major mode for editing Scala
(use-package scala-mode
  :straight t)


;; An Emacs mode for interacting with Scala sbt (Simple build tool) and projects
(use-package sbt-mode
  :straight t)


(provide 'on-demand/me-scala)
;;; me-scala.el ends here
