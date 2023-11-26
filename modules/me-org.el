;;; me-org.el --- Org related stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(defgroup minemacs-org nil
  "MinEmacs org-mode tweaks."
  :group 'minemacs)

(use-package me-org-extras
  :after org
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
  :after org
  :demand t)

(use-package engrave-faces
  :straight t
  :after org)

;; Org export
(use-package ox-hugo
  :straight t
  :after ox
  :demand t)

(use-package ox-extra
  :after ox
  :demand t
  :config
  (ox-extras-activate '(latex-header-blocks ignore-headlines)))

;; Other Org features
(use-package org-appear
  :straight t
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-inside-latex t)
  (org-appear-autokeywords t)
  (org-appear-autoentities t)
  (org-appear-autoemphasis t)
  (org-appear-autosubmarkers t)
  (org-appear-autolinks 'just-brackets)
  :config
  ;; For proper first-time setup, `org-appear--set-elements' needs to be run after other hooks have acted.
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
  (org-modern-checkbox nil) ;; Not that interesting! Maybe it depends on the used font
  (org-modern-todo-faces
   ;; Tweak colors, and force it to be monospaced, useful when using `mixed-pitch-mode'.
   '(("IDEA" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "goldenrod"))
     ("NEXT" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "IndianRed1"))
     ("STRT" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "OrangeRed"))
     ("WAIT" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "coral"))
     ("KILL" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "DarkGreen"))
     ("PROJ" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "LimeGreen"))
     ("HOLD" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "orange"))
     ("DONE" . (:inherit org-verbatim :weight semi-bold :foreground "black" :background "LightGray")))))

;; For latex fragments
(use-package org-fragtog
  :straight t
  :hook (org-mode . org-fragtog-mode)
  :custom
  (org-fragtog-preview-delay 0.2))

(use-package evil-org
  :straight t
  :hook (org-mode . evil-org-mode))

(use-package evil-org-agenda
  :after evil-org
  :demand t
  :config
  (evil-org-agenda-set-keys))


(provide 'me-org)

;;; me-org.el ends here
