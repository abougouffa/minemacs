;; me-evil.el --- MinEmacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


(use-package evil
  :straight t
  :custom
  (evil-want-C-i-jump nil)
  (evil-want-C-h-delete t) ;; C-h is backspace in insert state
  (evil-want-fine-undo t)
  (evil-want-keybinding nil)
  (evil-want-integration t)
  (evil-want-Y-yank-to-eol t)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-kill-on-visual-paste nil)
  (evil-respect-visual-line-mode t)
  :config
  ;; Better but may cause problems with org-fold
  ;; https://github.com/emacs-evil/evil/issues/1630
  (evil-select-search-module 'evil-search-module 'evil-search)
  (evil-mode 1)
  ;; Ask for a buffer when splitting windows
  (with-eval-after-load 'consult
    (dolist (fn '(evil-window-split evil-window-vsplit))
      (advice-add
       fn :after
       (defun +evil--cunsult-buffer-after-window-split-a (&rest _)
         (consult-buffer))))))


(use-package evil-collection
  :after evil minemacs-loaded
  :straight t
  :config
  (defvar +evil-collection-modes
    (seq-filter
     (lambda (mode)
       (not (memq mode '(evil-mc ;; Default bindings for evil-mc are messy
                         elisp-mode)))) ;; I don't like gz for ielm, I like gr though
     evil-collection-mode-list))

  (evil-collection-init +evil-collection-modes)

  ;; Define find references for elisp mode
  (with-eval-after-load 'elisp-mode
    (when evil-collection-want-find-usages-bindings
      (evil-collection-define-key 'normal 'emacs-lisp-mode-map
        "gr" 'xref-find-references))))

(use-package evil-nerd-commenter
  :straight t
  :after evil minemacs-loaded
  :general
  (+map-key "gc" #'evilnc-comment-operator))


(provide 'me-evil)
