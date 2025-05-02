;;; me-window.el --- Windows and frames -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2022-10-15
;; Last modified: 2025-05-02

;;; Commentary:

;; Window configuration for special windows.
;; This section inspired by the article "Demystifying Emacs’s Window Manager" found here:
;; https://www.masteringemacs.org/article/demystifying-emacs-window-manager

;;; Code:

;; Quickly switch windows in Emacs
(use-package ace-window
  :straight t
  :bind (("M-o" . ace-window))
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))


(provide 'me-window)

;;; me-window.el ends here
