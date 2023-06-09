;; me-core-ui.el --- MinEmacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

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

(use-package nerd-icons
  :straight t
  :config
  ;; Show .m files as matlab/octave files (integral icon)
  (setcdr (assoc "m" nerd-icons-extension-icon-alist)
          '(nerd-icons-mdicon "nf-md-math_integral_box" :face nerd-icons-orange)))

(use-package doom-themes
  :straight t
  :config
  (doom-themes-org-config))

(use-package dashboard
  :straight t
  :after evil evil-collection
  :demand t
  :when (not (bound-and-true-p +dashboard-disable))
  :init
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
  (dashboard-startup-banner (concat minemacs-assets-dir "images/minemacs.png"))
  :config
  ;; Ensure setting the keybindings before openning the dashboard
  (evil-collection-dashboard-setup)

  ;; Avoid openning the dashboard when Emacs starts with an open file.
  (unless (cl-some #'buffer-file-name (buffer-list))
    (dashboard-open)))

(use-package doom-modeline
  :straight t
  :hook (minemacs-after-startup . doom-modeline-mode)
  :custom
  (doom-modeline-height 35)
  (doom-modeline-bar-width 8)
  (doom-modeline-buffer-encoding 'nondefault)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-enable-word-count t)
  (doom-modeline-continuous-word-count-modes
   '(markdown-mode gfm-mode org-mode rst-mode latex-mode tex-mode text-mode))
  :config
  ;; HACK: Add some padding to the right
  (doom-modeline-def-modeline 'main
    '(bar workspace-name window-number modals matches follow buffer-info
      remote-host buffer-position word-count parrot selection-info)
    '(compilation objed-state misc-info persp-name battery grip irc mu4e gnus
      github debug repl lsp minor-modes input-method indent-info buffer-encoding
      major-mode process vcs checker time "  ")))


(provide 'me-core-ui)

;;; me-core-ui.el ends here
