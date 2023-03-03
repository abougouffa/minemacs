;;; me-multi-cursors.el --- Programming stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>

(use-package iedit
  :straight t
  :after minemacs-lazy
  :demand t
  :preface
  (+fn-inhibit-messages! iedit-update-key-bindings))

(use-package evil-iedit-state
  :straight t
  :after iedit evil
  :demand t)

(use-package evil-mc
  :straight t
  :after minemacs-loaded evil-collection
  :demand t
  :config
  ;; Use gz instead of gr
  (setcdr
   evil-mc-key-map ;; Redefine the default binting
   (let ((map (make-sparse-keymap)))
     (evil-define-key* '(normal visual) map
       (kbd "gz") evil-mc-cursors-map
       (kbd "M-n") 'evil-mc-make-and-goto-next-cursor
       (kbd "M-p") 'evil-mc-make-and-goto-prev-cursor
       (kbd "C-n") 'evil-mc-make-and-goto-next-match
       (kbd "C-t") 'evil-mc-skip-and-goto-next-match
       (kbd "C-p") 'evil-mc-make-and-goto-prev-match)
     map))

  ;; https://github.com/gabesoft/evil-mc/issues/70
  (add-hook
   'evil-mc-after-cursors-deleted
   (defun +evil-mc--fix-yank-h ()
     (setq evil-was-yanked-without-register t)))

  (global-evil-mc-mode 1)

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
         (funcall fn)))))

  ;; Custom commands to execute with evil-mc
  (dolist (fn '((backward-kill-word)
                (corfu-complete . evil-mc-execute-default-complete)
                (undo-fu-only-undo . evil-mc-execute-default-undo)
                (undo-fu-only-redo . evil-mc-execute-default-redo)
                (evil-delete-back-to-indentation . evil-mc-execute-default-call)
                (evil-escape . evil-mc-execute-default-evil-normal-state) ;; C-g
                (evil-numbers/inc-at-pt-incremental)
                (evil-numbers/dec-at-pt-incremental)
                (evil-digit-argument-or-evil-beginning-of-visual-line
                 (:default . evil-mc-execute-default-call)
                 (visual . evil-mc-execute-visual-call))
                (ess-smart-comma . evil-mc-execute-call)
                (evil-org-delete . evil-mc-execute-default-evil-delete)))
    (setf (alist-get (car fn) evil-mc-custom-known-commands)
          (if (and (cdr fn) (listp (cdr fn)))
              (cdr fn)
            (list (cons :default
                        (or (cdr fn)
                            #'evil-mc-execute-default-call-with-count)))))))


(provide 'me-multi-cursors)
