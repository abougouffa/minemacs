;;; me-anzu.el --- anzu -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:


;; From Doom Emacs
(use-package anzu
  :straight t
  :custom
  (anzu-cons-mode-line-p nil) ; We manage our own modeline segments
  :config
  ;; Ensure anzu state is cleared when searches & iedit are done
  (add-hook 'iedit-mode-end-hook #'anzu--reset-status)
  (advice-add #'evil-force-normal-state :before #'anzu--reset-status)
  ;; Fix matches segment mirroring across all buffers
  (mapc #'make-variable-buffer-local
        '(anzu--total-matched anzu--current-position anzu--state anzu--cached-count
          anzu--cached-positions anzu--last-command anzu--last-isearch-string anzu--overflow-p)))

(use-package evil-anzu
  :straight t
  :unless (+package-disabled-p 'evil 'me-evil)
  :hook (evil-mode . global-anzu-mode))


(provide 'obsolete/me-anzu)

;;; me-anzu.el ends here
