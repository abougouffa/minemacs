;;; me-docs.el --- Documents (PDF, EPUB, DOC...) -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(use-package pdf-tools
  :straight t
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :hook (minemacs-build-functions . pdf-tools-install)
  :custom
  (pdf-view-display-size 'fit-page)
  (pdf-view-image-relief 2)
  (pdf-view-use-scaling t))

(use-package nov
  :straight t
  :mode ("\\.[eE][pP][uU][bB]\\'" . nov-mode)
  :custom
  (nov-save-place-file (concat minemacs-local-dir "nov/save-place.el"))
  :config
  (+nmap! :keymaps 'nov-mode-map
    "RET" #'nov-scroll-up))

(defconst +tuntox-available-p (and (executable-find "tuntox") t))
(defconst +stunnel-available-p (and (executable-find "stunnel") t))

(use-package crdt
  :straight t
  :when (or +tuntox-available-p +stunnel-available-p)
  :custom
  (crdt-tuntox-password-in-url t)
  (crdt-use-tuntox +tuntox-available-p)
  (crdt-use-stunnel +stunnel-available-p))

(defconst +easydraw-available-p (+emacs-features-p 'rsvg 'zlib 'libxml2))

(use-package edraw
  :straight (:host github :repo "misohena/el-easydraw")
  :when +easydraw-available-p
  :custom
  (edraw-ui-state-file (+directory-ensure minemacs-local-dir "edraw/ui-state.el"))
  (edraw-shape-picker-entries-file (concat minemacs-local-dir "edraw/shape-picker-entries.el")))

(use-package edraw-org
  :hook (org-mode . edraw-org-setup-default)
  :when +easydraw-available-p)

(use-package markdown-mode
  :straight t
  :mode ("README\\.md\\'" . gfm-mode)
  :custom
  (markdown-hide-markup t)
  (markdown-enable-html t)
  (markdown-enable-math t)
  :config
  (+map-local! :keymaps 'markdown-mode-map
    "l"  '(nil :wk "link")
    "ll" #'markdown-insert-link
    "e"  #'markdown-export))

(use-package pandoc-mode
  :straight t
  :hook (markdown-mode . conditionally-turn-on-pandoc))


(provide 'me-docs)

;;; me-docs.el ends here
