;;; me-ui.el --- UI stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;; Nerd Font icons for Emacs
(use-package nerd-icons
  :ensure t
  :hook (minemacs-build-functions . nerd-icons-install-fonts)
  :config
  ;; Show .m files as Matlab/Octave files (integral icon)
  (setcdr (assoc "m" nerd-icons-extension-icon-alist) '(nerd-icons-mdicon "nf-md-math_integral_box" :face nerd-icons-orange)))


;; A megapack of themes for Emacs
(use-package doom-themes
  :ensure t
  :config
  (with-eval-after-load 'org
    (doom-themes-org-config))
  ;; Enable blinking modeline on errors (`visible-bell')
  (+with-delayed-1! (doom-themes-visual-bell-config)))


;; Colourful and legible themes for GNU Emacs
(use-package ef-themes
  :ensure t)


;; A fancy and fast mode-line inspired by minimalism design
(use-package doom-modeline
  :ensure t
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
  :ensure t
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
  :vc (:url "https://github.com/ichernyshovvv/enlight")
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
  :vc (:url "https://github.com/emacsmirror/lacarte")
  :bind ([f10] . lacarte-execute-menu-command))


;; Display "^L" page breaks as tidy horizontal lines
(use-package page-break-lines
  :ensure t
  :hook ((prog-mode text-mode special-mode) . page-break-lines-mode))


;; Pulse highlight on demand or after select functions
(use-package pulsar
  :ensure t
  :hook (minemacs-first-file . pulsar-global-mode)
  :custom
  (pulsar-iterations 6)
  (pulsar-pulse-region t)
  (pulsar-pulse-on-window-change t)
  (pulsar-region-face 'pulsar-green)
  (pulsar-highlight-face 'pulsar-cyan)
  (pulsar-region-change-face 'pulsar-red)
  (pulsar-window-change-face 'pulsar-yellow)
  :config
  (cl-callf append pulsar-pulse-functions '(what-cursor-position)))


;; Dim the font color of text in surrounding sections
(use-package focus
  :ensure t)


;; Minor mode to automatically balance window margins
(use-package olivetti
  :ensure t)


;; Integrate `nerd-icons' with `ibuffer'
(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))


;; Integrate `nerd-icons' with `archive-mode', `tar-mode', `dired-mode', and `ztree'
(use-package nerd-icons-multimodal
  :vc (:url "https://github.com/abougouffa/nerd-icons-multimodal")
  :hook ((archive-mode tar-mode dired-mode ztree-mode) . nerd-icons-multimodal-mode))


;; Extra font lock rules for a more colourful `dired'
(use-package diredfl
  :ensure t
  :hook (dired-mode . diredfl-mode)
  :config
  (cl-callf append diredfl-compressed-extensions '(".zst" ".rar" ".7z" ".cab" ".arc" ".zoo")))


;; Extra colors for `Info-mode'
(use-package info-colors
  :ensure t
  :hook (Info-selection . info-colors-fontify-node))


;; Format buffers visually without modification
(use-package virtual-format
  :vc (:url "https://github.com/abougouffa/virtual-format"))


;; A collection of opinionated keyboard-driven user interfaces for various built-in Emacs modes
(use-package casual
  :ensure t
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
  :ensure t
  :bind ("M-g a" . casual-avy-tmenu))


;; An opinionated `transient' menu for `symbol-overlay'
(use-package casual-symbol-overlay
  :ensure t
  :bind (:package symbol-overlay :map symbol-overlay-map ("C-o" . casual-symbol-overlay-tmenu)))


(provide 'me-ui)

;;; me-ui.el ends here
