;; me-nano.el --- N Λ N O Emacs UI tweaks -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;; Layout
(setq window-divider-default-right-width 24
      window-divider-default-places 'right-only
      x-underline-at-descent-line t)

;; (setq default-frame-alist (+alist-set 'internal-border-width 20 default-frame-alist))

(when os/mac
  (+setq-hook! term-mode buffer-display-table (make-display-table)))

(use-package nano-theme
  :straight (:host github :repo "rougier/nano-theme")
  :init
  ;; Set `nano-light' as the default MinEmacs theme
  (+set-standard-value 'minemacs-theme 'nano-light))

(use-package nano-modeline
  :straight (:host github :repo "rougier/nano-modeline")
  :after minemacs-loaded
  :demand t
  :config
  ;; Disable the default mode-line
  (setq-default mode-line-format nil)
  (nano-modeline-text-mode t)
  (dolist (mode '(prog text org pdf-view mu4e-headers mu4e-view mu4e-compose elfeed-show elfeed-search elpher term eat xwidget-webkit messages-buffer org-capture org-agenda))
    (let ((mode-hook (intern (format "%s-mode-hook" mode)))
          (hook-func (intern (format "nano-modeline-%s-mode" mode))))
      (add-hook mode-hook hook-func))))

(use-package nano-vertico
  :straight (:host github :repo "rougier/nano-vertico")
  :hook (minemacs-after-startup . nano-vertico-mode))


(provide 'me-nano)

;;; me-nano.el ends here
