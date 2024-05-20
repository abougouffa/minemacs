;;; me-yasnippet.el --- Yasnippet config -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package yasnippet
  :straight t
  :hook (minemacs-lazy . yas-global-mode)
  :init
  (defvar yas-verbosity 2)
  :custom
  (yas-snippet-dirs nil)
  (yas-triggers-in-field t))

(use-package yasnippet-capf
  :straight t
  :after cape yasnippet
  :demand t
  :hook ((prog-mode text-mode conf-mode) . +cape-yasnippet--setup-h)
  :config
  ;; To avoid auto-expanding snippets
  (plist-put yasnippet-capf--properties :exit-function #'always)
  (defun +cape-yasnippet--setup-h ()
    (when (bound-and-true-p yas-minor-mode)
      (add-to-list 'completion-at-point-functions #'yasnippet-capf))))

(use-package yasnippet-snippets
  :straight t)

(use-package doom-snippets
  :straight (:host github :repo "hlissner/doom-snippets" :files ("*.el" "*")))

(use-package license-snippets
  :straight t
  :after yasnippet
  :init
  (license-snippets-init))


(provide 'obsolete/me-yasnippet)

;;; me-yasnippet.el ends here
