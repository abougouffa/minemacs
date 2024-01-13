;; me-evil-escape.el --- Escape without ESC -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(use-package evil-escape
  :straight t
  :hook (evil-mode . evil-escape-mode)
  :custom
  ;; The default "fd" interfere with the "f" (bound to `evil-snipe-f') binding.
  (evil-escape-key-sequence "kj")
  (evil-escape-unordered-key-sequence t) ; "kj" or "jk"
  :config
  (with-eval-after-load 'evil-mc
    (push '(evil-escape (:default . evil-mc-execute-default-evil-normal-state)) evil-mc-custom-known-commands)

    (defun +evil-mc-evil-escape-move-back-fake-cursors ()
      (unless (bolp) (backward-char)))

    (advice-add
     'evil-escape-func :before
     (defun +evil-mc--evil-escape-fix:before-a ()
       (when (evil-mc-has-cursors-p)
         (evil-mc-pause-cursors)
         (run-with-idle-timer
          0 nil
          (lambda ()
            (evil-mc-resume-cursors)
            (let ((evil-mc-command '((:name . +evil-mc-evil-escape-move-back-fake-cursors))))
              (evil-mc-execute-for-all)))))))

    (add-to-list
     'evil-mc-custom-known-commands
     '(+evil-mc-evil-escape-move-back-fake-cursors
       (:default . evil-mc-execute-default-call)))))

(provide 'obsolete/me-evil-escape)

;;; me-evil-escape.el ends here
