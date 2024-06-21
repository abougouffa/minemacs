;;; me-zotxt.el --- Zotero integration -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package zotxt
  :straight t
  :when (executable-find "zotero")
  :init
  (+map-local! :keymaps 'org-mode-map
    "z" #'org-zotxt-mode)
  (+map-local! :keymaps 'markdown-mode-map
    "z" #'zotxt-citekey-mode))


(provide 'obsolete/me-zotxt)
;;; me-zotxt.el ends here
