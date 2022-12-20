;;; me-modeling.el --- Mechanical modeling stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


(use-package scad-mode
  :straight t
  :defer t
  :config
  (+eglot-register 'scad-mode '("openscad-lsp" "--stdio"))
  (+map-local :keymaps 'scad-mode-map
    "p" #'scad-preview))


(provide 'me-modeling)
