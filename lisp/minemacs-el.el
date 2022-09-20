;;; vc.el --- Lisps -*- lexical-binding: t; -*-


(use-package magit
  :straight t
  :commands (magit magit-status))

(use-package evil-magit
  :straight t
  :after magit)
