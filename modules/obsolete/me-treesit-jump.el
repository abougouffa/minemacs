;;; me-treesit-jump.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn@cneebg.pbz")
;; Created: 2025-08-21
;; Last modified: 2025-08-21

;;; Commentary:

;;; Code:


;; Jump around your source code in emacs using `treesit' and `avy'
(use-package treesit-jump
  :straight (:host github :repo "abougouffa/treesit-jump" :branch "enhancements" :files (:defaults "treesit-queries"))
  :when (featurep 'feat/tree-sitter))


(provide 'obsolete/me-treesit-jump)
;;; me-treesit-jump.el ends here
