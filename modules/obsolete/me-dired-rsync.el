;;; me-dired-rsync.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-07-19
;; Last modified: 2025-07-19

;;; Commentary:

;;; Code:


;; Asynchronous "rsync" from `dired'
(use-package dired-rsync
  :straight (:files ("dired-rsync.el" "dired-rsync-transient.el"))
  :bind ( :package dired :map dired-mode-map
          ("C-c C-r" . dired-rsync)
          ("C-c C-x" . dired-rsync-transient)))


(provide 'obsolete/me-dired-rsync)
;;; me-dired-rsync.el ends here
