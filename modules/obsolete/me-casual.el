;;; me-casual.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-12-11
;; Last modified: 2025-12-11

;;; Commentary:

;;; Code:


;; A collection of opinionated keyboard-driven user interfaces for various built-in Emacs modes
(use-package casual
  :straight (:host github :repo "kickingvegas/casual")
  :bind ("C-o" . casual-editkit-main-tmenu)
  :bind (:package bookmark :map bookmark-bmenu-mode-map ("C-o" . casual-bookmarks-tmenu))
  :bind (:package calc :map calc-mode-map ("C-o" . casual-calc-tmenu))
  :bind (:package calendar :map calendar-mode-map ("C-o" . casual-calendar))
  :bind (:package dired :map dired-mode-map ("C-o" . casual-dired-tmenu))
  :bind (:package esh-mode :map eshell-mode-map ("C-o" . casual-eshell-tmenu))
  :bind (:package ibuffer :map ibuffer-mode-map ("C-o" . casual-ibuffer-tmenu))
  :bind (:package image-mode :map image-mode-map ("C-o" . casual-image-tmenu))
  :bind (:package info :map Info-mode-map ("C-o" . casual-info-tmenu))
  :bind (:package isearch :map isearch-mode-map ("C-o" . casual-isearch-tmenu))
  :bind (:package make-mode :map makefile-mode-map ("C-o" . casual-make-tmenu))
  :bind (:package man :map Man-mode-map ("C-o" . casual-man-tmenu))
  :bind (:package help-mode :map help-mode-map ("C-o" . casual-help-tmenu))
  :bind (:package org-agenda :map org-agenda-mode-map ("C-o" . casual-agenda-tmenu))
  :bind (:package re-builder :map reb-mode-map ("C-o" . casual-re-builder-tmenu)))


;; An opinionated `transient' menu for `avy'
(use-package casual-avy
  :straight t
  :bind ("M-g a" . casual-avy-tmenu))


(provide 'obsolete/me-casual)
;;; me-casual.el ends here
