;;; me-ui.el --- UI stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;; Nerd Font icons for Emacs
(use-package nerd-icons
  :straight t
  :hook (minemacs-build-functions . nerd-icons-install-fonts)
  :config
  ;; Show .m files as Matlab/Octave files (integral icon)
  (setcdr (assoc "m" nerd-icons-extension-icon-alist) '(nerd-icons-mdicon "nf-md-math_integral_box" :face nerd-icons-orange)))


;; A megapack of themes for Emacs
(use-package doom-themes
  :straight t
  :config
  (with-eval-after-load 'org
    (doom-themes-org-config))
  ;; Enable blinking modeline on errors (`visible-bell')
  (+with-delayed-1! (doom-themes-visual-bell-config)))


;; Colourful and legible themes for GNU Emacs
(use-package ef-themes
  :straight t)


;; A fancy and fast mode-line inspired by minimalism design
(use-package doom-modeline
  :straight t
  :hook (minemacs-lazy . doom-modeline-mode)
  :custom
  (doom-modeline-time-icon nil)
  (doom-modeline-buffer-encoding 'nondefault)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-total-line-number t)
  (doom-modeline-enable-word-count t)
  (doom-modeline-continuous-word-count-modes '(markdown-mode markdown-ts-mode gfm-mode org-mode rst-mode latex-mode tex-mode)))


;; Show current command and its key in the mode line
(use-package keycast
  :straight t
  :commands (keycast-doom-modeline-mode)
  :config
  (define-minor-mode keycast-doom-modeline-mode
    "Show keycast in `doom-modeline'."
    :global t
    (if keycast-doom-modeline-mode
        (progn (add-hook 'pre-command-hook 'keycast--update t)
               (add-to-list 'global-mode-string '("" keycast-mode-line " ")))
      (remove-hook 'pre-command-hook 'keycast--update)
      (cl-callf2 delete '("" keycast-mode-line " ") global-mode-string))))


;; Highly customizable startup screen for Emacs
(use-package enlight
  :straight (:host github :repo "ichernyshovvv/enlight")
  :custom
  (enlight-content
   (enlight-menu
    '(("Org Mode"
       ("Org-Agenda (today)" (org-agenda nil "a") "a")
       ("Org directory" (dired org-directory) "o"))
      ("Projects"
       ("Switch to project" project-switch-project "p"))
      ("Desktop / Session"
       ("Restore session" desktop-read "r")
       ("Restore session from file" +desktop-read-session "R")))))
  :init
  (if minemacs-started-with-extra-args-p
      (enlight-open)
    (setq initial-buffer-choice #'enlight)))


;; Execute menu items as commands, with completion
(use-package lacarte
  :straight t
  :bind ([f10] . lacarte-execute-menu-command))


;; Use a variable pitch, keeping fixed pitch where it's sensible
(use-package mixed-pitch
  :straight t
  :custom
  (mixed-pitch-variable-pitch-cursor nil)
  :config
  (setq
   mixed-pitch-fixed-pitch-faces
   (delete-dups
    (append
     mixed-pitch-fixed-pitch-faces
     '(font-lock-comment-delimiter-face font-lock-comment-face org-block
       org-block-begin-line org-block-end-line org-cite org-cite-key
       org-document-info-keyword org-done org-drawer org-footnote org-formula
       org-inline-src-block org-latex-and-related org-link org-code org-column
       org-column-title org-date org-macro org-meta-line org-property-value
       org-quote org-ref-cite-face org-sexp-date org-special-keyword org-src
       org-table org-tag org-tag-group org-todo org-verbatim org-verse)))))


;; Display "^L" page breaks as tidy horizontal lines
(use-package page-break-lines
  :straight t
  :hook ((prog-mode text-mode special-mode) . page-break-lines-mode))


;; Pulse highlight on demand or after select functions
(use-package pulsar
  :straight t
  :hook (minemacs-first-file . pulsar-global-mode)
  :hook ((next-error xref-after-return) . pulsar-pulse-line) ; only pulse, don't recenter
  :hook ((consult-after-jump imenu-after-jump xref-after-jump) . pulsar-recenter-center) ; pulse and recenter
  :hook ((consult-after-jump imenu-after-jump xref-after-jump xref-after-return) . pulsar-reveal-entry) ; reveal if hidden
  :custom
  (pulsar-face 'pulsar-red)
  :config
  (cl-callf append pulsar-pulse-functions
    '(what-cursor-position scroll-up-command scroll-down-command kill-whole-line yank-from-kill-ring yank yank-pop)))


;; Dim the font color of text in surrounding sections
(use-package focus
  :straight t)


;; Minor mode to automatically balance window margins
(use-package olivetti
  :straight t)


;; Integrate `nerd-icons' with `ibuffer'
(use-package nerd-icons-ibuffer
  :straight t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))


;; Integrate `nerd-icons' with `archive-mode', `tar-mode' and `dired-mode'
(use-package nerd-icons-multimodal
  :straight (:host github :repo "abougouffa/nerd-icons-multimodal")
  :hook ((archive-mode tar-mode dired-mode) . nerd-icons-multimodal-mode))


;; Extra font lock rules for a more colourful `dired'
(use-package diredfl
  :straight t
  :hook (dired-mode . diredfl-mode)
  :config
  (cl-callf append diredfl-compressed-extensions '(".zst" ".rar" ".7z" ".cab" ".arc" ".zoo")))


;; Extra colors for `Info-mode'
(use-package info-colors
  :straight t
  :hook (Info-selection . info-colors-fontify-node))


;; Format buffers visually without modification
(use-package virtual-format
  :straight (:host github :repo "abougouffa/virtual-format"))


;; A collection of opinionated keyboard-driven user interfaces for various built-in Emacs modes
(use-package casual
  :straight (:host github :repo "kickingvegas/casual")
  :bind ("C-o" . casual-editkit-main-tmenu)
  :bind (:package isearch :map isearch-mode-map ([f2] . casual-isearch-tmenu))
  :bind (:package dired :map dired-mode-map ([f2] . casual-dired-tmenu))
  :bind (:package info :map Info-mode-map ([f2] . casual-info-tmenu))
  :bind (:package calc :map calc-mode-map ([f2] . casual-calc-tmenu))
  :bind (:package ibuffer :map ibuffer-mode-map ([f2] . casual-ibuffer-tmenu))
  :bind (:package re-builder :map reb-mode-map ([f2] . casual-re-builder-tmenu))
  :bind (:package bookmark :map bookmark-bmenu-mode-map ([f2] . casual-bookmarks-tmenu))
  :bind (:package org-agenda :map org-agenda-mode-map ([f2] . casual-agenda-tmenu)))


;; An opinionated `transient' menu for `avy'
(use-package casual-avy
  :straight t
  :bind ("M-g a" . casual-avy-tmenu))


;; An opinionated `transient' menu for `symbol-overlay'
(use-package casual-symbol-overlay
  :straight t
  :bind (:package symbol-overlay :map symbol-overlay-map ("C-o" . casual-symbol-overlay-tmenu)))


(provide 'me-ui)

;;; me-ui.el ends here
