;;; me-super-save.el --- Automatically save files -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package super-save
  :straight t
  :hook (minemacs-first-file . super-save-mode)
  :custom
  (super-save-silent t)
  (super-save-all-buffers t)
  (super-save-auto-save-when-idle t)
  (super-save-delete-trailing-whitespace 'except-current-line)
  :config
  ;; Additional triggers
  (cl-callf append super-save-triggers
    '(magit magit-status winner-undo winner-redo find-file)))


(provide 'obsolete/me-super-save)
;;; me-super-save.el ends here
