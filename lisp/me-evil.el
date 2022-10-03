;; -*- lexical-binding: t; -*-

(use-package evil
  :straight t
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-i-jump nil
        evil-respect-visual-line-mode t
        evil-want-C-h-delete t ;; C-h is backspace in insert state
        evil-want-Y-yank-to-eol t
        evil-want-fine-undo t)
  :config
  (evil-set-undo-system 'undo-redo)

  ;; Enable Evil globally
  (evil-mode 1))


(use-package evil-collection
  :after evil
  :straight t
  :config
  (evil-collection-init))


(use-package evil-nerd-commenter
  :after evil
  :straight t)


(use-package evil-mc
  :after evil
  :straight t
  :config
  ;; Enable multi cursors
  (global-evil-mc-mode 1))


(use-package evil-escape
  :after evil
  :straight t)


(provide 'me-evil)
