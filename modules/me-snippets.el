;;; me-snippets.el --- Snippets -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-11-17
;; Last modified: 2026-06-01

;;; Commentary:

;;; Code:


(use-package tempel
  :straight t
  :custom
  (tempel-trigger-prefix "<") ;; Require trigger prefix before template name when completing.
  (tempel-path (list (concat minemacs-assets-dir "templates/tempel/*.eld")
                     (concat minemacs-config-dir "templates/tempel/*.eld")))
  :bind (("M-\"" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert)
         :map tempel-map
         ("TAB" . tempel-next)
         ("<backtab>" . tempel-previous))
  :hook ((prog-mode text-mode) . +tempel-setup-capf-h)
  :hook (prog-mode . tempel-abbrev-mode)
  :config
  (defun +tempel-setup-capf-h ()
    (add-hook 'completion-at-point-functions #'tempel-complete -90 t)))


(use-package tempel-collection
  :straight t)


(provide 'me-snippets)
;;; me-snippets.el ends here
