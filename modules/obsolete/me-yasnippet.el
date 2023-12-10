;;; me-yasnippet.el --- Yasnippet config -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa and contributors

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

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

(use-package cape-yasnippet
  :straight (:host github :repo "elken/cape-yasnippet")
  :after cape yasnippet
  :demand t
  :hook ((prog-mode text-mode conf-mode) . +cape-yasnippet--setup-h)
  :config
  ;; To avoid auto-expanding snippets
  (plist-put cape-yasnippet--properties :exit-function #'always)
  (defun +cape-yasnippet--setup-h ()
    (when (bound-and-true-p yas-minor-mode)
      (add-to-list 'completion-at-point-functions #'cape-yasnippet))))

(use-package yasnippet-snippets
  :straight t
  :after yasnippet
  :demand t)

(use-package doom-snippets
  :straight (:host github :repo "hlissner/doom-snippets" :files ("*.el" "*"))
  :after yasnippet
  :demand t)

(use-package license-snippets
  :straight t
  :after yasnippet
  :demand t
  :config
  (license-snippets-init))


(provide 'obsolete/me-yasnippet)

;;; me-yasnippet.el ends here
