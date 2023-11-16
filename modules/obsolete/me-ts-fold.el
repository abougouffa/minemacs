;;; me-ts-fold.el --- Tree-sitter code folding -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(if (not (+emacs-features-p 'tree-sitter))
    (use-package ts-fold ; tree-sitter
      :straight (:host github :repo "emacs-tree-sitter/ts-fold")
      :after tree-sitter
      :demand t
      :init
      (global-ts-fold-mode 1))

  ;; To avoid installing `tree-sitter' as the used `ts-fold' fork uses the
  ;; built-in `treesit'
  (push 'tree-sitter straight-built-in-pseudo-packages)
  (use-package ts-fold ; treesit
    :straight (:host github :repo "abougouffa/ts-fold" :branch "andrew-sw/treesit-el-support")
    :when (+emacs-features-p 'tree-sitter)
    ;; TEMP: Normally, we don't have to hook it explicitly, but it seems that
    ;; `global-ts-fold-mode' isn't working correctly on `yaml-ts-mode'
    :hook (yaml-ts-mode . ts-fold-mode)
    :after treesit treesit-auto
    :demand t
    :init
    (global-ts-fold-mode 1)))


(provide 'obsolete/me-ts-fold)

;;; me-ts-fold.el ends here
