;;; me-smartparens.el --- Smartparens -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package smartparens
 :straight t
 :hook (minemacs-after-startup . smartparens-global-mode)
 :init
 ;; From Doom Emacs, disable expensive navigation features.
 (+setq-hook! smartparens-mode
   sp-navigate-skip-match nil
   sp-navigate-consider-sgml-tags nil)
 :config
 (sp-local-pair 'org-mode "$" "$" :unless '(sp-point-after-word-p))
 (with-eval-after-load 'evil-mc
   ;; Make evil-mc cooperate with smartparens better
   (let ((vars (cdr (assq :default evil-mc-cursor-variables))))
     (unless (memq (car sp--mc/cursor-specific-vars) vars)
       (setcdr (assq :default evil-mc-cursor-variables) (append vars sp--mc/cursor-specific-vars))))))

;; Default `smartparens' configuration (for example, do not complete a single
;; quote)
(use-package smartparens-config
  :after smartparens
  :demand)


(provide 'obsolete/me-smartparens)

;;; me-smartparens.el ends here
