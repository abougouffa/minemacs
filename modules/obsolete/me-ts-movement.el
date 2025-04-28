;;; me-ts-movement.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Last modified: 2025-04-19

;;; Commentary:

;;; Code:


;; Move and edit code blocks based on tree-sitter AST
(use-package ts-movement
  :straight (:host github :repo "haritkapadia/ts-movement")
  :when (+emacs-options-p 'tree-sitter)
  :hook ((prog-mode conf-mode) . +ts-movement-maybe)
  :init
  (defun +ts-movement-maybe ()
    "Enable `ts-movement-mode' when if `major-mode' is a trees-sitter mode."
    (run-with-timer 1.0 nil (lambda () (when (treesit-parser-list) (ts-movement-mode 1)))))
  :config
  (with-eval-after-load 'transient
    (transient-define-prefix +ts-movement-transient ()
      "Transient for ts-movement."
      [[("d" "delete-overlay-at-point" tsm/delete-overlay-at-point :transient t)
        ("D" "clear-overlays-of-type" tsm/clear-overlays-of-type :transient t)
        ("C-b" "backward-overlay" tsm/backward-overlay :transient t)
        ("C-f" "forward-overlay" tsm/forward-overlay :transient t)
        ("c" "tsm/mc/mark-all-overlays" tsm/mc/mark-all-overlays :transient t)]
       [("a" "node-start" tsm/node-start :transient t)
        ("e" "node-end" tsm/node-end :transient t)
        ("b" "node-prev" tsm/node-prev :transient t)
        ("f" "node-next" tsm/node-next :transient t)]
       [("p" "node-parent" tsm/node-parent :transient t)
        ("n" "node-child" tsm/node-child :transient t)
        ("N" "node-children" tsm/node-children :transient t)
        ("s" "node-children-of-type" tsm/node-children-of-type :transient t)
        ("m" "node-mark" tsm/node-mark :transient t)]]
      [("Q" "Quit" ignore :transient t)])))


(provide 'obsolete/me-ts-movement)
;;; me-ts-movement.el ends here
