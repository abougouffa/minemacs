;;; me-checkers.el --- Syntax checking -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package flymake-collection
  :straight t
  :init
  (+hook-once! 'prog-mode-hook (flymake-collection-hook-setup))
  :custom
  (flymake-collection-hook-inherit-config t)
  :config
  ;; Activate more checkers for Python
  (setf (alist-get '(python-mode python-ts-mode) flymake-collection-hook-config nil nil 'equal)
        (append (when (executable-find "pycodestyle") '(flymake-collection-pycodestyle))
                (when (executable-find "mypy") '(flymake-collection-mypy))
                (when (executable-find "pylint") '(flymake-collection-pylint))
                (when (executable-find "ruff") '(flymake-collection-ruff))
                '((flymake-collection-flake8 :disabled t)))))

(use-package flymake-collection-define
  :straight flymake-collection
  :autoload flymake-collection-define flymake-collection-define-rx)

(use-package me-flymake-extras
  :after flymake-collection
  :init
  (let ((checkers (assoc '(python-mode python-ts-mode) flymake-collection-hook-config 'equal)))
    (setcdr checkers (append (cdr checkers)
                             (when (executable-find "pyre") '(flymake-collection-pyre))
                             (when (executable-find "bandit") '(flymake-collection-bandit))
                             (when (executable-find "codespell") '(flymake-collection-codespell)))))

  (let ((checkers (assoc '(c++-mode c++-ts-mode) flymake-collection-hook-config 'equal)))
    (setcdr checkers (append (cdr checkers)
                             (when (executable-find "clang-tidy") '(flymake-collection-clang-tidy))
                             (when (executable-find "codespell") '(flymake-collection-codespell))))))

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
