;;; me-modeling.el --- Mechanical modeling stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


(use-package scad-mode
  :straight t
  :config
  (+map-local :keymaps 'scad-mode-map
    "p" #'scad-preview)

  (with-eval-after-load 'all-the-icons
    (add-to-list
     'all-the-icons-extension-icon-alist
     '("scad" all-the-icons-fileicon "openscad" :height 0.9 :face all-the-icons-yellow)))

  (with-eval-after-load 'apheleia
    (push '(scad-mode . clang-format) apheleia-mode-alist))

  (+eglot-register 'scad-mode '("openscad-lsp" "--stdio")))


(provide 'me-modeling)
