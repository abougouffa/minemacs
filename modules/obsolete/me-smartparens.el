;;; me-smartparens.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-07-28
;; Last modified: 2025-07-28

;;; Commentary:

;;; Code:


;; Minor mode for Emacs that deals with parens pairs and tries to be smart about it
(use-package smartparens
  :straight t
  :hook (minemacs-lazy . smartparens-global-mode)
  :custom
  (sp-ignore-modes-list '(minibuffer-inactive-mode)) ; Enable in `minibuffer-mode'
  :config
  (require 'smartparens-config)

  ;; In minibuffer, don't complete ' and `
  (sp-local-pair 'minibuffer-mode "'" nil :actions nil)
  (sp-local-pair 'minibuffer-mode "`" nil :actions nil)
  (sp-local-pair 'minibuffer-mode "`" "'" :when '(sp-in-string-p))
  (sp-local-pair 'org-mode "$" "$" :unless '(sp-point-after-word-p))

  ;; Silence some harmless but annoying echo-area spam
  (dolist (key '(:unmatched-expression :no-matching-tag)) (setf (alist-get key sp-message-alist) nil)))


(provide 'obsolete/me-smartparens)
;;; me-smartparens.el ends here
