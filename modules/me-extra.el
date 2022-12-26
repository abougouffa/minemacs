;; -*- lexical-binding: t; -*-

(use-package better-jumper
  :straight t
  :hook (minemacs-after-startup . better-jumper-mode)
  :init
  ;; Map extra mouse buttons to jump forward/backward
  (global-set-key [mouse-8] #'better-jumper-jump-backward)
  (global-set-key [mouse-9] #'better-jumper-jump-forward))


(provide 'me-extra)
