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
  (setcdr (assoc "m" nerd-icons-extension-icon-alist) '(nerd-icons-mdicon "nf-md-math_integral_box" :face nerd-icons-orange))
  (defun +nerd-icons-icon (name &rest args)
    "Generic function to get icons by NAME, with ARGS."
    (if-let* ((variant (and (string-match "^nf-\\([[:alnum:]]+\\)-" name) (match-string 1 name)))
              (fn (intern (format "nerd-icons-%sicon" variant)))
              ((fboundp fn)))
        (apply fn (cons name args))
      (error "Cannot detect the function which provides %S" name))))


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


;; Display "^L" page breaks as tidy horizontal lines
(use-package page-break-lines
  :straight t
  :hook ((prog-mode text-mode special-mode) . page-break-lines-mode))


;; Pulse highlight on demand or after select functions
(use-package pulsar
  :straight (:host github :repo "protesilaos/pulsar")
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


;; Integrate `nerd-icons' with `ibuffer'
(use-package nerd-icons-ibuffer
  :straight t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))


;; Integrate `nerd-icons' with `archive-mode', `tar-mode', `dired-mode', and `ztree'
(use-package nerd-icons-multimodal
  :straight (:host github :repo "abougouffa/nerd-icons-multimodal")
  :hook ((archive-mode tar-mode dired-mode ztree-mode) . nerd-icons-multimodal-mode))


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
  :bind (:package image-mode :map image-mode-map ([f2] . casual-image-tmenu))
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
