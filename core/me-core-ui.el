;; me-core-ui.el --- MinEmacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;; Disable previously enabled custom themes before enabling a new one.
(advice-add
 'load-theme :before
 (defun +theme--disable-previous-themes:before-a (&rest _)
   "Disable previously enabled themes before enabling the new one."
   (mapc #'disable-theme custom-enabled-themes)))

(use-package nerd-icons
  :straight t
  :hook (minemacs-build-functions . nerd-icons-install-fonts)
  :config
  ;; Show .m files as matlab/octave files (integral icon)
  (setcdr (assoc "m" nerd-icons-extension-icon-alist)
          '(nerd-icons-mdicon "nf-md-math_integral_box" :face nerd-icons-orange))
  (when (and (display-graphic-p) (not (+font-installed-p nerd-icons-font-family)))
    (nerd-icons-install-fonts 'dont-ask)))

(use-package doom-themes
  :straight t
  :config
  (with-eval-after-load 'org
    (doom-themes-org-config)))

(use-package dashboard
  :straight t
  :after evil evil-collection
  :demand t
  :unless (bound-and-true-p +dashboard-disable)
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
  ;; Ensure setting the keybindings before opening the dashboard
  (evil-collection-dashboard-setup)

  ;; Avoid opening the dashboard when Emacs starts with an open file.
  (unless (cl-some #'buffer-file-name (buffer-list))
    (dashboard-open)))

(use-package doom-modeline
  :straight t
  :unless (memq 'me-nano minemacs-modules)
  :hook (minemacs-after-startup . doom-modeline-mode)
  :custom
  (doom-modeline-height 28)
  (doom-modeline-bar-width 8)
  (doom-modeline-time-icon nil)
  (doom-modeline-buffer-encoding 'nondefault)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-enable-word-count t)
  (doom-modeline-continuous-word-count-modes
   '(markdown-mode gfm-mode org-mode rst-mode latex-mode tex-mode text-mode))
  :config
  ;; HACK: Add some padding to the right
  (doom-modeline-def-modeline 'main
    '(eldoc bar workspace-name window-number modals matches follow buffer-info
      remote-host buffer-position word-count parrot selection-info)
    '(compilation objed-state misc-info persp-name battery grip irc mu4e gnus
      github debug repl lsp minor-modes input-method indent-info buffer-encoding
      major-mode process vcs checker time "  ")))

(use-package solaire-mode
  :straight t
  :hook (minemacs-after-startup . solaire-global-mode)
  :config
  (dolist (face '(mode-line mode-line-active mode-line-inactive mode-line-emphasis))
    (setf (alist-get face solaire-mode-remap-alist) nil)))


(provide 'me-core-ui)

;;; me-core-ui.el ends here
