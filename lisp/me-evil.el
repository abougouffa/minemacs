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
  (evil-mode 1))


(use-package evil-collection
  :after evil
  :straight t
  :config
  (evil-collection-init))


(use-package evil-nerd-commenter
  :after (evil minemacs-loaded)
  :straight t)


(use-package evil-mc
  :straight t
  :after evil
  :general
  (me-map-def
    :states '(normal visual)
    :keymaps 'evil-mc-key-map
    "gz" '(nil :which-key "evil-mc")
    "gzd" #'evil-mc-make-and-goto-next-match
    "gzD" #'evil-mc-make-and-goto-prev-match
    "gzs" #'evil-mc-skip-and-goto-next-match
    "gzS" #'evil-mc-skip-and-goto-prev-match
    "gzc" #'evil-mc-skip-and-goto-next-cursor
    "gzC" #'evil-mc-skip-and-goto-prev-cursor
    "gzj" #'evil-mc-make-cursor-move-next-line
    "gzk" #'evil-mc-make-cursor-move-prev-line
    "gzm" #'evil-mc-make-all-cursors
    "gzn" #'evil-mc-make-and-goto-next-cursor
    "gzN" #'evil-mc-make-and-goto-last-cursor
    "gzp" #'evil-mc-make-and-goto-prev-cursor
    "gzP" #'evil-mc-make-and-goto-first-cursor
    "gzq" #'evil-mc-undo-all-cursors)
  (me-map-def
    :states 'visual
    :keymaps 'evil-mc-key-map
    "gzi" #'evil-mc-make-cursor-in-visual-selection-beg
    "gza" #'evil-mc-make-cursor-in-visual-selection-end)
  :config
  (add-hook 'prog-mode-hook 'turn-on-evil-mc-mode)
  (add-hook 'text-mode-hook 'turn-on-evil-mc-mode)
  (add-hook 'magit-mode-hook 'turn-off-evil-mc-mode)

  ;; Add support to repeat these commands when prefixed with a number
  (dolist (cmd '(evil-mc-make-and-goto-first-cursor
                 evil-mc-make-and-goto-last-cursor
                 evil-mc-make-and-goto-prev-cursor
                 evil-mc-make-and-goto-next-cursor
                 evil-mc-skip-and-goto-prev-cursor
                 evil-mc-skip-and-goto-next-cursor
                 evil-mc-make-and-goto-prev-match
                 evil-mc-make-and-goto-next-match
                 evil-mc-skip-and-goto-prev-match
                 evil-mc-skip-and-goto-next-match))
    (advice-add
     cmd :around
     (lambda (fn)
       (dotimes (i (if (integerp current-prefix-arg) current-prefix-arg 1))
         (funcall fn))))))

(use-package evil-escape
  :after (evil minemacs-loaded)
  :straight t
  :config
  (evil-escape-mode 1))


(provide 'me-evil)
