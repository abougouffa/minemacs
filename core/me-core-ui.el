;; me-core-ui.el --- MinEmacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


(defun +theme--tweaks-h (&rest _args)
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

;; Icons
(use-package all-the-icons
  :straight t
  :defer t
  :config
  ;; Show .m files as matlab/octave files
  (setcdr (assoc "m" all-the-icons-extension-icon-alist)
          (cdr (assoc "matlab" all-the-icons-extension-icon-alist))))

(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-one-light t))

;; Modeline
(use-package doom-modeline
  :straight t
  :custom
  (doom-modeline-height 45)
  (doom-modeline-bar-width 8)
  (doom-modeline-time-icon nil)
  (doom-modeline-buffer-encoding 'nondefault)
  (doom-modeline-unicode-fallback t)
  :config
  ;; Add padding
  (doom-modeline-def-modeline 'main
    '(bar workspace-name window-number modals matches follow buffer-info
      remote-host buffer-position word-count parrot selection-info)
    '(objed-state misc-info persp-name battery grip irc mu4e gnus github debug
      repl lsp minor-modes input-method indent-info buffer-encoding major-mode
      process vcs checker time "   "))

  (doom-modeline-mode 1))


(provide 'me-core-ui)
