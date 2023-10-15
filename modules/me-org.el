;;; me-org.el --- Org related stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(defgroup minemacs-org nil
  "MinEmacs org-mode tweaks."
  :group 'minemacs)

(use-package me-org-extras
  :after minemacs-first-org-file org
  :demand t
  :config
  (+org-extras-outline-path-setup)
  (+org-extras-latex-classes-setup)
  (+org-extras-responsive-images-setup)
  (+org-extras-equation-numbering-setup)
  (+org-extras-multifiles-document-setup)
  (+org-extras-pretty-latex-fragments-setup)
  (+org-extras-lower-case-keywords-and-properties-setup))

(use-package org-contrib
  :straight t
  :after minemacs-first-org-file org
  :demand t)

(use-package engrave-faces
  :straight t
  :after minemacs-first-org-file org)

;; Org export
(use-package ox-hugo
  :straight t
  :after minemacs-first-org-file ox
  :demand t)

(use-package ox-pandoc
  :straight t
  :after minemacs-first-org-file ox
  :demand t)

(use-package ox-extra
  :after minemacs-first-org-file ox
  :demand t
  :config
  (ox-extras-activate '(latex-header-blocks ignore-headlines)))

;; Other Org features
(use-package org-appear
  :straight t
  :after minemacs-first-org-file
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-inside-latex t)
  (org-appear-autokeywords t)
  (org-appear-autoentities t)
  (org-appear-autoemphasis t)
  (org-appear-autosubmarkers t)
  (org-appear-autolinks 'just-brackets)
  :config
  ;; for proper first-time setup, `org-appear--set-elements'
  ;; needs to be run after other hooks have acted.
  (run-at-time nil nil #'org-appear--set-elements))

(use-package org-modern
  :straight t
  :hook (org-mode . org-modern-mode)
  :hook (org-agenda-finalize . org-modern-agenda)
  :custom-face
  ;; Force monospaced font for tags
  (org-modern-tag ((t (:inherit org-verbatim :weight regular :foreground "black" :background "LightGray" :box "black"))))
  :custom
  (org-modern-star '("◉" "○" "◈" "◇" "✳" "◆" "✸" "▶"))
  (org-modern-table-vertical 5)
  (org-modern-table-horizontal 2)
  (org-modern-list '((?+ . "➤") (?- . "–") (?* . "•")))
  (org-modern-block-fringe nil)
  (org-modern-todo-faces
   ;; Tweak colors, and force it to be monospaced, useful when using
   ;; mixed-pitch-mode.
   '(("IDEA" . (:inherit org-verbatim :weight semi-bold
                :foreground "white" :background "goldenrod"))
     ("NEXT" . (:inherit org-verbatim :weight semi-bold
                :foreground "white" :background "IndianRed1"))
     ("STRT" . (:inherit org-verbatim :weight semi-bold
                :foreground "white" :background "OrangeRed"))
     ("WAIT" . (:inherit org-verbatim :weight semi-bold
                :foreground "white" :background "coral"))
     ("KILL" . (:inherit org-verbatim :weight semi-bold
                :foreground "white" :background "DarkGreen"))
     ("PROJ" . (:inherit org-verbatim :weight semi-bold
                :foreground "white" :background "LimeGreen"))
     ("HOLD" . (:inherit org-verbatim :weight semi-bold
                :foreground "white" :background "orange"))
     ("DONE" . (:inherit org-verbatim :weight semi-bold
                :foreground "black" :background "LightGray")))))

;; For latex fragments
(use-package org-fragtog
  :straight t
  :hook (org-mode . org-fragtog-mode)
  :custom
  (org-fragtog-preview-delay 0.2))

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
      '(:enabled nil
        :width nil
        :center-text nil)
    "Variable to hold `visual-fill-column-mode' parameters")

  (defun +org-present--on-h ()
    (setq-local
     face-remapping-alist
     '((default (:height 1.5) variable-pitch)
       (header-line (:height 2.0) variable-pitch)
       (org-document-title (:height 2.0) org-document-title)
       (org-code (:height 1.55) org-code)
       (org-verbatim (:height 1.55) org-verbatim)
       (org-block (:height 1.25) org-block)
       (org-block-begin-line (:height 0.7) org-block)))
    ;; (org-present-big)
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
    (setq-local
     face-remapping-alist
     '((default default default)))
    ;; (org-present-small)
    (org-remove-inline-images)
    (org-present-show-cursor)
    (org-present-read-write)
    (visual-fill-column-mode -1)
    (unless (plist-get +org-present--vcm-params :enabled)
      (setq-local visual-fill-column-width (plist-get +org-present--vcm-params :width)
                  visual-fill-column-center-text (plist-get +org-present--vcm-params :center-text))
      (visual-fill-column-mode 1))))

(use-package evil-org
  :straight t
  :after minemacs-first-org-file
  :hook (org-mode . evil-org-mode))

(use-package evil-org-agenda
  :after minemacs-first-org-file evil-org
  :demand t
  :config
  (evil-org-agenda-set-keys))


(provide 'me-org)

;;; me-org.el ends here
