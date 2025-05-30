;;; me-experimental.el --- Experimental packages -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-12-11
;; Last modified: 2025-05-19

;;; Commentary:

;;; Code:


;; Highlight inactive code regions with eglot power (mainly C/C++ preprocessor directives)
(use-package eglot-inactive-regions
  :straight t
  :commands (eglot-inactive-regions-mode))


;; Extra non-standard functionalities for Eglot
(use-package eglot-x
  :straight (:host github :repo "nemethf/eglot-x")
  :commands (eglot-x-setup))


;; Work seamlessly with GitHub gists from Emacs
(use-package igist
  :straight t
  :config
  (advice-add ; BUG+FIX: Don't save the Gist unless it has been modified
   'igist-save-gist-buffer :around
   (satch-defun igist--check-if-modified:around-a (orig-fn buffer &optional callback)
     (when (igist-gist-modified-p buffer)
       (funcall orig-fn buffer callback)))))


;; The Emacs Gerrit Experience
(use-package gee
  :straight (:host bitbucket :repo "olanilsson/gee"))


;; Gerrit integration with Magit
(use-package magit-gerrit
  :straight (:host github :repo "darcylee/magit-gerrit" :fork "abougouffa/magit-gerrit")
  :after magit
  :demand
  :custom
  (magit-gerrit-popup-prefix "_"))


;; Gerrit integration from ChromiumOS development utils
(use-package gerrit
  :straight (chromiumos-dev-utils :type git :repo "https://chromium.googlesource.com/chromiumos/platform/dev-util" :files ("contrib/emacs/gerrit/*")))


;; Transient menus to use some "repo" commands within Magit
(use-package repo-transient
  :straight chromiumos-dev-utils
  :commands (repo-main-menu))


;; A Dynamic Module for WebKit, aka a fully fledged browser inside Emacs
(use-package webkit
  :straight `( :host github :repo "akirakyle/emacs-webkit"
               :files (:defaults "*.js" "*.css" "*.so")
               :pre-build ,(if (featurep 'os/win)
                               '(message "The `webkit' module won't build on Windows.")
                             '("make")))
  :when (and (featurep 'feat/modules) (not (featurep 'os/win))))


;; A Dynamic Module for WebKit, aka a fully fledged browser inside Emacs
(use-package webkit-dark
  :straight webkit
  :when (and (featurep 'feat/modules) (not (featurep 'os/win)))
  :bind (:map webkit-mode-map ("C-c d" . webkit-dark-toggle)))


(provide 'me-experimental)
;;; me-experimental.el ends here
