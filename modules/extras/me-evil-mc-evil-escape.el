;;; me-evil-mc-evil-escape.el --- Hackish integration of `evil-mc' with `evil-escape' -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")


;; HACK: Fix the integration with `evil-escape' when in insert state. By
;; default, when pressing "fd" in evil-mc insert state, the first letter "f"
;; gets inserted at all fake cursors. This hack fixes this behavior.
;; Taken from: github.com/gabesoft/evil-mc/issues/41#issuecomment-890887060
(+after-load! '(:all evil-escape evil-mc)
  (defun +evil-mc--evil-escape-move-back-fake-cursors ()
    (unless (bolp) (backward-char)))

  (advice-add
   'evil-escape-func :before
   (defun +evil-mc--evil-escape-fix-a ()
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
   '(+evil-mc--evil-escape-move-back-fake-cursors
     (:default . evil-mc-execute-default-call))))


(provide 'me-evil-mc-evil-escape)
