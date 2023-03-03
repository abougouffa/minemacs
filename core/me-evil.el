;; me-evil.el --- Emacs as Vim! -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


(use-package evil
  :straight t
  :hook (minemacs-after-startup . evil-mode)
  :preface
  (setq evil-want-keybinding nil)
  :custom
  (evil-want-C-i-jump nil)
  (evil-want-fine-undo t)
  (evil-want-Y-yank-to-eol t)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-kill-on-visual-paste nil)
  (evil-respect-visual-line-mode t)
  (evil-ex-interactive-search-highlight 'selected-window)
  :config
  ;; Ask for a buffer when splitting windows
  (with-eval-after-load 'consult
    (dolist (fn '(evil-window-split evil-window-vsplit))
      (advice-add
       fn :after
       (defun +evil--cunsult-buffer-after-window-split-a (&rest _)
         (consult-buffer))))))

(use-package evil-collection
  :straight t
  :after evil minemacs-loaded
  :demand t
  :config
  (evil-collection-init
   (seq-filter
    (lambda (mode)
      (not (memq mode '(evil-mc ;; Default bindings for evil-mc are messy
                        elisp-mode)))) ;; I don't like gz for ielm, I like gr though
    evil-collection-mode-list))

  ;; Define find references for elisp mode
  (with-eval-after-load 'elisp-mode
    (when evil-collection-want-find-usages-bindings
      (evil-collection-define-key 'normal 'emacs-lisp-mode-map
        "gr" 'xref-find-references))))

(use-package evil-snipe
  :straight t
  :hook (minemacs-after-startup . evil-snipe-mode)
  :hook (minemacs-after-startup . evil-snipe-override-mode)
  :custom
  (evil-snipe-scope 'buffer)
  (evil-snipe-smart-case t)
  (evil-snipe-auto-scroll t))

(use-package evil-numbers
  :straight t
  :init
  (+map-key
    :states '(normal)
    "g+" #'evil-numbers/inc-at-pt
    "g=" #'evil-numbers/inc-at-pt
    "g-" #'evil-numbers/dec-at-pt)
  (+map-key
    :states '(visual)
    "g+" #'evil-numbers/inc-at-pt-incremental
    "g=" #'evil-numbers/inc-at-pt-incremental
    "g-" #'evil-numbers/dec-at-pt-incremental))

(use-package evil-nerd-commenter
  :straight t
  :init
  (+map-key "gc" #'evilnc-comment-operator))

;; https://github.com/redguardtoo/emacs.d/blob/8ea127c69cd6e5d6dbbe2c1dce91131c4a4c0cd2/lisp/init-evil.el#L236
;; Press kj to escape from evil-insert-state and everything else in Emacs. It’s much more efficient than ESC in Vim or C-g in Emacs
(use-package evil-escape
  :straight t
  :after evil
  :init (evil-escape-mode +1)
  :config
  (setq evil-escape-key-sequence "kj"
        evil-escape-delay 0.3))

(provide 'me-evil)
