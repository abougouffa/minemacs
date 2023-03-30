;; me-core-ui.el --- MinEmacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")


(defun +theme--tweaks-h (&optional _)
  "Use smaller font (75% of the default) for line numbers in graphic mode."
  (when (display-graphic-p)
    (set-face-attribute
     'line-number nil
     :background (face-attribute 'default :background)
     :height (truncate (* 0.75 (face-attribute 'default :height)))
     :weight 'semi-light)
    (set-face-attribute
     'line-number-current-line nil
     :height (truncate (* 0.75 (face-attribute 'default :height)))
     :weight 'bold)))

;; Apply tweaks
(add-hook 'after-init-hook #'+theme--tweaks-h)
(add-hook 'enable-theme-functions #'+theme--tweaks-h)

;; Save enabled theme
(add-hook
 'enable-theme-functions
 (defun +theme--save-enabled-theme-h (theme)
   "Save the enabled theme to `minemacs-theme'.
Useful for keeping track of the enabled theme."
   (setq minemacs-theme theme)))

;; Disable previously enabled custom themes before enabling a new one.
(advice-add
 'load-theme :before
 (defun +theme--disable-previous-themes-a (&rest _)
   "Disable previously enabled themes before enabling the new one."
   (mapc #'disable-theme custom-enabled-themes)))

(use-package modus-themes
  :straight (:host github :repo "protesilaos/modus-themes")
  :config
  ;; In all of the following, WEIGHT is a symbol such as `semibold',
  ;; `light', `bold', or anything mentioned in `modus-themes-weights'.
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-mixed-fonts nil
        modus-themes-variable-pitch-ui nil
        modus-themes-custom-auto-reload t

        ;; Options for `modus-themes-prompts' are either nil (the
        ;; default), or a list of properties that may include any of those
        ;; symbols: `italic', `WEIGHT'
        modus-themes-prompts '(semi-bold)

        ;; The `modus-themes-completions' is an alist that reads two
        ;; keys: `matches', `selection'.  Each accepts a nil value (or
        ;; empty list) or a list of properties that can include any of
        ;; the following (for WEIGHT read further below):
        ;; `matches'   :: `underline', `italic', `WEIGHT'
        ;; `selection' :: `underline', `italic', `WEIGHT'
        modus-themes-completions
        '((matches   . (extrabold))
          (selection . (semibold text-also)))

        modus-themes-org-blocks 'gray-background ; {nil,'gray-background,'tinted-background}

        ;; The `modus-themes-headings' is an alist: read the manual's
        ;; node about it or its doc string.  Basically, it supports
        ;; per-level configurations for the optional use of
        ;; `variable-pitch' typography, a height value as a multiple of
        ;; the base font size (e.g. 1.5), and a `WEIGHT'.
        modus-themes-headings
        '((1                . (1.4))
          (2                . (1.3))
          (3                . (1.2))
          (agenda-date      . (1.2))
          (agenda-structure . (light 1.5))
          (t                . (1.1)))

        modus-themes-common-palette-overrides
        `(;; Customize the mode-line colors
          (bg-mode-line-active bg-blue-intense)
          (fg-mode-line-active fg-main)

          ;; From the section "Make the mode line borderless"
          (border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)

          ;; From the section "Make matching parenthesis more or less intense"
          (bg-paren-match bg-blue-intense)
          (underline-paren-match unspecified)

          ;; Links
          (underline-link border)
          (underline-link-visited border)
          (underline-link-symbolic border)

          ;; Comments are yellow, strings are green
          (comment yellow-cooler)
          (string green-warmer)

          ;; And expand the preset here. Note that the ,@ works because we use
          ;; the backtick for this list, instead of a straight quote.
          ,@modus-themes-preset-overrides-faint))

  ;; Load the theme of your choice.
  (load-theme 'modus-operandi-tinted t))

(use-package all-the-icons
  :straight t
  :config
  ;; Show .m files as matlab/octave files
  (setcdr (assoc "m" all-the-icons-extension-icon-alist)
          (cdr (assoc "matlab" all-the-icons-extension-icon-alist))))

(use-package doom-themes
  :straight t)

(use-package apropospriate-theme
  :straight t)

(use-package dashboard
  :straight t
  :init
  (setq initial-buffer-choice #'dashboard-open)
  (+map! "oD" #'dashboard-open)
  :custom
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-center-content t)
  (dashboard-banner-ascii "MinEmacs")
  (dashboard-banner-logo-title "Welcome to MinEmacs!")
  (dashboard-items '((recents . 5) (projects . 5) (bookmarks . 5)))
  (dashboard-image-banner-max-width 600)
  (dashboard-projects-backend 'project-el)
  (dashboard-startup-banner (concat minemacs-assets-dir "images/minemacs.png")))

(use-package doom-modeline
  :straight t
  :hook (minemacs-after-startup . doom-modeline-mode)
  :custom
  (doom-modeline-height 35)
  (doom-modeline-bar-width 8)
  (doom-modeline-time-icon nil)
  (doom-modeline-buffer-encoding 'nondefault)
  (doom-modeline-unicode-fallback t)
  :config
  ;; HACK: Add some padding to the right
  (doom-modeline-def-modeline 'main
    '(bar workspace-name window-number modals matches follow buffer-info
      remote-host buffer-position word-count parrot selection-info)
    '(compilation objed-state misc-info persp-name battery grip irc mu4e gnus
      github debug repl lsp minor-modes input-method indent-info buffer-encoding
      major-mode process vcs checker time "  ")))


(provide 'me-core-ui)
