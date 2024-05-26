;; me-core-ui.el --- MinEmacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;; Disable previously enabled custom themes before enabling a new one.
(advice-add
 'load-theme :before
 (satch-defun +theme--disable-previous-themes:before-a (&rest _)
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

(use-package doom-modeline
  :straight t
  :unless (memq 'me-nano minemacs-modules)
  :hook (minemacs-after-startup . doom-modeline-mode)
  :custom
  (doom-modeline-bar-width 1)
  (doom-modeline-time-icon nil)
  (doom-modeline-buffer-encoding 'nondefault)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-total-line-number t)
  (doom-modeline-enable-word-count t)
  (doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode rst-mode latex-mode tex-mode text-mode))
  :custom-face
  ;; Hide the modeline bar
  (doom-modeline-bar ((t (:inherit mode-line :background unspecified))))
  (doom-modeline-bar-inactive ((t (:inherit mode-line :background unspecified)))))

(use-package enlight
  :straight (:host github :repo "ichernyshovvv/enlight")
  :when (>= emacs-major-version 29) ; TEMP+BUG: There is an issue with Emacs 28
  :custom
  (enlight-content
   (enlight-menu
    '(("Org Mode"
       ("Org-Agenda (today)" (org-agenda nil "a") "a"))
      ("Projects"
       ("Switch to project" project-switch-project "p")))))
  :init
  (setq initial-buffer-choice #'enlight))


(provide 'me-core-ui)

;;; me-core-ui.el ends here
