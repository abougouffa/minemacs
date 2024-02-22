;;; me-makefile-executor.el --- Makefile executor (replaced with projection-multi) -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package makefile-executor
  :straight t
  :hook (makefile-mode . makefile-executor-mode)
  :init
  (+map!
    "pm" '(nil :wk "makefile-executor")
    "pmm" #'makefile-executor-execute-project-target
    "pml" #'makefile-executor-execute-last)
  (+map-local! :keymaps 'makefile-mode-map
    "pmt" #'makefile-executor-execute-target
    "pmb" #'makefile-executor-execute-dedicated-buffer))


(provide 'obsolete/me-makefile-executor)

;;; me-makefile-executor.el ends here
