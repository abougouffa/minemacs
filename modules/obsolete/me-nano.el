;; me-nano.el --- N Λ N O Emacs UI tweaks -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-01-01
;; Last modified: 2025-05-01

;;; Commentary:

;;; Code:

;; Layout
(setq window-divider-default-right-width 24
      window-divider-default-places 'right-only
      x-underline-at-descent-line t)

;; (setq default-frame-alist (+alist-set 'internal-border-width 20 default-frame-alist))

(when (featurep 'os/mac)
  (+setq-hook! term-mode buffer-display-table (make-display-table)))

(use-package nano-theme
  :straight (:host github :repo "rougier/nano-theme")
  :init
  (setq minemacs-theme 'nano-light))

(use-package nano-modeline
  :straight (:host github :repo "rougier/nano-modeline")
  :after minemacs-loaded
  :demand
  :custom
  (nano-modeline-position #'nano-modeline-footer)
  :config
  ;; Disable the default mode-line
  (setq-default mode-line-format nil)
  (nano-modeline-text-mode t)
  ;; Install hooks
  (add-hook 'prog-mode-hook            #'nano-modeline-prog-mode)
  (add-hook 'text-mode-hook            #'nano-modeline-text-mode)
  (add-hook 'org-mode-hook             #'nano-modeline-org-mode)
  (add-hook 'pdf-view-mode-hook        #'nano-modeline-pdf-mode)
  (add-hook 'mu4e-headers-mode-hook    #'nano-modeline-mu4e-headers-mode)
  (add-hook 'mu4e-view-mode-hook       #'nano-modeline-mu4e-message-mode)
  (add-hook 'mu4e-compose-mode-hook    #'nano-modeline-mu4e-compose-mode)
  (add-hook 'elfeed-show-mode-hook     #'nano-modeline-elfeed-entry-mode)
  (add-hook 'elfeed-search-mode-hook   #'nano-modeline-elfeed-search-mode)
  (add-hook 'elpher-mode-hook          #'nano-modeline-elpher-mode)
  (add-hook 'term-mode-hook            #'nano-modeline-term-mode)
  (add-hook 'eat-mode-hook             #'nano-modeline-eat-mode)
  (add-hook 'xwidget-webkit-mode-hook  #'nano-modeline-xwidget-mode)
  (add-hook 'messages-buffer-mode-hook #'nano-modeline-message-mode)
  (add-hook 'org-capture-mode-hook     #'nano-modeline-org-capture-mode)
  (add-hook 'org-agenda-mode-hook      #'nano-modeline-org-agenda-mode))

(use-package nano-vertico
  :straight (:host github :repo "rougier/nano-vertico")
  :hook (minemacs-lazy . nano-vertico-mode)
  :disabled)


(provide 'obsolete/me-nano)
;;; me-nano.el ends here
