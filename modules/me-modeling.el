;;; me-modeling.el --- Mechanical modeling stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package scad-mode
  :straight t
  :config
  (+map-local! :keymaps 'scad-mode-map
    "p" #'scad-preview)

  (with-eval-after-load 'nerd-icons
    (add-to-list
     'nerd-icons-extension-icon-alist
     '("scad" nerd-icons-mdicon "nf-md-cad_file" :face nerd-icons-yellow)))

  (with-eval-after-load 'apheleia-formatters
    (push '(scad-mode . clang-format) apheleia-mode-alist))

  (+eglot-register 'scad-mode '("openscad-lsp" "--stdio")))

(use-package modelica-mode
  :straight t
  :mode "\\.mo\\'")


(provide 'me-modeling)

;;; me-modeling.el ends here
