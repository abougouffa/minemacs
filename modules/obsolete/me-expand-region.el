;;; me-expand-region.el --- expand-region config for non-treesitter build -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(use-package expand-region
  :straight t
  :init
  (+vmap! "v" #'er/expand-region))

(provide 'obsolete/me-expand-region)

;;; me-expand-region.el ends here
