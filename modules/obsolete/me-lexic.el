;;; me-lexic.el --- Offline dictionary -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package lexic
  :straight t
  :when (executable-find "sdcv")
  :init
  (+map! :infix "s"
    "l" #'lexic-search-word-at-point
    "L" #'lexic-search)
  :config
  (+nvmap! :keymaps 'lexic-mode-map
    "q" #'lexic-return-from-lexic
    "RET" #'lexic-search-word-at-point
    "a" #'outline-show-all
    "h" `(,(+cmdfy! (outline-hide-sublevels 3)) :wk "Hide sublevels")
    "o" #'lexic-toggle-entry
    "n" #'lexic-next-entry
    "N" `(,(+cmdfy! (lexic-next-entry t)) :wk "Last entry")
    "p" #'lexic-previous-entry
    "P" `(,(+cmdfy! (lexic-previous-entry t)) :wk "First entry")
    "E" `(,(+cmdfy!
            (lexic-return-from-lexic)
            (switch-to-buffer (lexic-get-buffer)))
          :wk "Expand")
    "M" `(,(+cmdfy!
            (lexic-return-from-lexic)
            (lexic-goto-lexic))
          :wk "Minimise")
    "C-p" #'lexic-search-history-backwards
    "C-n" #'lexic-search-history-forwards
    "/" `(,(+cmdfy! (call-interactively #'lexic-search)) :wk "Search")))

(provide 'obsolete/me-lexic)

;;; me-lexic.el ends here
