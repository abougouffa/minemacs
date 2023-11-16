;;; me-multi-cursors.el --- Programming stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(use-package iedit
  :straight t
  :after minemacs-first-file
  :demand t
  :preface
  (+fn-inhibit-messages! iedit-update-key-bindings))

(use-package evil-iedit-state
  :straight t
  :after iedit evil
  :demand t
  :config
  ;; Use the `iedit' key to trigger `evil-iedit-state/iedit-mode'.
  (keymap-global-set (key-description iedit-toggle-key-default) 'evil-iedit-state/iedit-mode))

(use-package evil-mc
  :straight t
  :hook (minemacs-first-file . global-evil-mc-mode)
  :config
  ;; Use "gz" instead of "gr", this last is mapped to `xref-find-references' in
  ;; some programming modes.
  (evil-define-key* '(normal visual) evil-mc-key-map (kbd "gr") nil)
  (evil-define-key* '(normal visual) evil-mc-key-map (kbd "gz") evil-mc-cursors-map)

  ;; Add support to repeat these commands when prefixed with a number
  (dolist (cmd '(evil-mc-make-and-goto-first-cursor evil-mc-make-and-goto-last-cursor
                 evil-mc-make-and-goto-prev-cursor evil-mc-make-and-goto-next-cursor
                 evil-mc-skip-and-goto-prev-cursor evil-mc-skip-and-goto-next-cursor
                 evil-mc-make-and-goto-prev-match evil-mc-make-and-goto-next-match
                 evil-mc-skip-and-goto-prev-match evil-mc-skip-and-goto-next-match))
    (advice-add
     cmd :around
     (lambda (fn)
       (dotimes (i (if (integerp current-prefix-arg) current-prefix-arg 1))
         (funcall fn)))))

  ;; Custom commands to execute with `evil-mc'
  (setq
   evil-mc-custom-known-commands
   '((backward-kill-word (:default . evil-mc-execute-default-call-with-count))
     (evil-delete-back-to-indentation (:default . evil-mc-execute-default-call))
     (undo-fu-only-redo (:default . evil-mc-execute-default-redo))
     (undo-fu-only-undo (:default . evil-mc-execute-default-undo))
     (corfu-complete (:default . evil-mc-execute-default-complete))
     (evil-numbers/dec-at-pt-incremental (:default . evil-mc-execute-default-call-with-count))
     (evil-numbers/inc-at-pt-incremental (:default . evil-mc-execute-default-call-with-count))
     (evil-org-delete (:default . evil-mc-execute-default-evil-delete))
     (ess-smart-comma (:default . evil-mc-execute-call))
     (evil-digit-argument-or-evil-beginning-of-visual-line
      (:default . evil-mc-execute-default-call)
      (visual . evil-mc-execute-visual-call)))))

(use-package evil-multiedit
  :straight t
  :after iedit evil
  :demand t
  :init
  (+nvmap! :infix "g"
    "ze" '(nil :wk "evil-multiedit")
    "zem" #'evil-multiedit-match-all
    "zed" #'evil-multiedit-match-and-next
    "zeD" #'evil-multiedit-match-and-prev
    "zes" #'evil-multiedit-match-symbol-and-next
    "zeS" #'evil-multiedit-match-symbol-and-prev
    "zen" #'evil-multiedit-next
    "zeN" #'evil-multiedit-prev
    "zet" #'evil-multiedit-toggle-or-restrict-region
    "zeT" #'evil-multiedit-toggle-marker-here
    "zeq" #'evil-multiedit-abort)
  :config
  (evil-multiedit-default-keybinds))


(provide 'me-multi-cursors)

;;; me-multi-cursors.el ends here
