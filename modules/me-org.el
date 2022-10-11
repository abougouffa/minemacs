;;; me-org.el --- Org related stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>

(use-package org
  :straight t
  :after minemacs-loaded ;; load Org after finishing Emacs startup
  :general
  (me-map-local :keymaps 'org-mode-map
    "l"  '(nil :which-key "link")
    "ll" '(org-insert-link :which-key "Insert link")
    "e" '(org-export-dispatch :which-key "Export dispatch"))
  :preface
  ;; Set to nil so we can detect user changes (in config.el)
  (defvar org-directory nil)
  (defvar org-id-locations-file nil)
  (defvar org-attach-id-dir nil)
  (defvar org-babel-python-command nil)
  (setq org-persist-directory (expand-file-name "org/persist/" minemacs-cache-dir)
        org-publish-timestamp-directory (expand-file-name "org/timestamps/" minemacs-cache-dir)
        org-preview-latex-image-directory (expand-file-name "org/latex/" minemacs-cache-dir)
        org-list-allow-alphabetical t)
  (let ((dir (expand-file-name "org/" minemacs-cache-dir)))
    (unless (file-directory-p dir)
      (mkdir dir t)))
  :custom
  (org-tags-column 0)
  (org-fold-catch-invisible-edits 'smart) ;; try not to accidently do weird stuff in invisible regions
  (org-export-with-sub-superscripts t) ;; don't treat lone _ / ^ as sub/superscripts, require _{} / ^{}
  (org-pretty-entities-include-sub-superscripts nil)
  (org-fontify-quote-and-verse-blocks t)
  (org-auto-align-tags nil)
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-ellipsis " ↩")
  (org-hide-leading-stars t)
  (org-use-property-inheritance t) ; it's convenient to have properties inherited
  (org-log-done 'time)             ; having the time an item is done sounds convenient
  (org-list-allow-alphabetical t)  ; have a. A. a) A) list bullets
  (org-export-in-background nil)   ; run export processes in external emacs process

  :config
  (require 'ox-latex nil :noerror)
  (require 'ob-tangle nil :noerror)

  (setq org-export-async-debug t) ;; Can be useful!

  (let ((size 1.3))
    (dolist (face '(org-level-1 org-level-2 org-level-3 org-level-4 org-level-5))
      (set-face-attribute face nil :weight 'semi-bold :height size)
      (setq size (max (* size 0.9) 1.0))))

  (require 'me-org-extras)
  (me-org-extras-setup))

(use-package org-contrib
  :straight t
  :after org)


(use-package org-appear
  :straight t
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autoemphasis t)
  (org-appear-autosubmarkers t)
  (org-appear-autolinks nil)
  :config
  ;; for proper first-time setup, `org-appear--set-elements'
  ;; needs to be run after other hooks have acted.
  (run-at-time nil nil #'org-appear--set-elements))


(use-package org-modern
  :straight t
  :hook (org-mode . org-modern-mode)
  :custom
  (org-modern-star '("◉" "○" "◈" "◇" "✳" "◆" "✸" "▶"))
  (org-modern-table-vertical 5)
  (org-modern-table-horizontal 2)
  (org-modern-list '((43 . "➤") (45 . "–") (42 . "•")))
  (org-modern-block-fringe nil))


;; for latex fragments
(use-package org-fragtog
  :straight t
  :hook (org-mode . org-fragtog-mode))


(provide 'me-org)

;;; me-org.el ends here
