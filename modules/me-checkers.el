;;; me-checkers.el --- Syntax checking -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package flymake-collection
  :straight (:host github :repo "abougouffa/flymake-collection" :branch "additional-checkers")
  :init
  (satch-add-hook 'prog-mode-hook #'flymake-collection-hook-setup nil nil :transient t)
  :custom
  (flymake-collection-hook-inherit-config t))

(use-package flymake-cppcheck
  :straight (:host github :repo "shaohme/flymake-cppcheck")
  :init
  (when (executable-find "cppcheck")
    (+add-hook! (c-mode c-ts-mode c++-mode c++-ts-mode) #'flymake-cppcheck-setup)))

(use-package flymenu
  :straight (:host github :repo "KarimAziev/flymenu"))

(use-package flymake-guile
  :straight (:source emacsmirror-mirror)
  :init
  (when (executable-find "guild") (add-hook 'scheme-mode-hook #'flymake-guile)))

(use-package flymake-plantuml
  :straight (:host github :repo "shaohme/flymake-plantuml")
  :hook (plantuml-mode . flymake-plantuml-setup))

(use-package flymake-relint
  :straight t
  :hook ((emacs-lisp-mode lisp-interaction-mode) . flymake-relint-setup))

(use-package flymake-pmd
  :straight (:host github :repo "rody/flymake-pmd"))


(provide 'me-checkers)

;;; me-checkers.el ends here
