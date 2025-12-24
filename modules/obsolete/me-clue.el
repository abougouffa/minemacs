;;; me-clue.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-07-09
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:


;; Connecting clues (a.k.a., note taking) while reading code
(use-package clue
  :vc (:url "https://github.com/AmaiKinono/clue")
  :custom
  (clue-project-root-function
   (if (fboundp '+citre-dominating-project-root) #'+citre-dominating-project-root #'clue-project-root))
  :hook (find-file . clue-auto-enable-clue-mode))


(provide 'obsolete/me-clue)
;;; me-clue.el ends here
