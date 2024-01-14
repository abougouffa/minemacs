;;; me-org-present.el --- Org Present -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package org-present
  :straight t
  :init
  (+map! "oP" :keymaps 'org-mode-map #'org-present)
  :hook (org-present-mode . +org-present--on-h)
  :hook (org-present-mode-quit . +org-present--off-h)
  :custom
  (org-present-text-scale 2.5)
  :config
  (defvar-local +org-present--vcm-params
      '(:enabled nil :width nil :center-text nil)
    "Variable to hold `visual-fill-column-mode' parameters")

  (defun +org-present--on-h ()
    (setq-local face-remapping-alist
                '((default (:height 1.5) variable-pitch)
                  (header-line (:height 2.0) variable-pitch)
                  (org-document-title (:height 2.0) org-document-title)
                  (org-code (:height 1.55) org-code)
                  (org-verbatim (:height 1.55) org-verbatim)
                  (org-block (:height 1.25) org-block)
                  (org-block-begin-line (:height 0.7) org-block)))
    (org-display-inline-images)
    (org-present-hide-cursor)
    (org-present-read-only)
    (when (bound-and-true-p visual-fill-column-mode)
      (+plist-push! +org-present--vcm-params
        :enabled visual-fill-column-mode
        :width visual-fill-column-width
        :center-text visual-fill-column-center-text))
    (setq-local visual-fill-column-width 120
                visual-fill-column-center-text t)
    (visual-fill-column-mode 1))

  (defun +org-present--off-h ()
    (setq-local face-remapping-alist '((default default default)))
    (org-remove-inline-images)
    (org-present-show-cursor)
    (org-present-read-write)
    (visual-fill-column-mode -1)
    (unless (plist-get +org-present--vcm-params :enabled)
      (setq-local visual-fill-column-width (plist-get +org-present--vcm-params :width)
                  visual-fill-column-center-text (plist-get +org-present--vcm-params :center-text))
      (visual-fill-column-mode 1))))


(provide 'obsolete/me-org-present)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; me-org-present.el ends here
