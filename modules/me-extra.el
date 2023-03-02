;;; me-extra.el --- Some extra functionalities -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


(use-package better-jumper
  :straight t
  :hook (minemacs-after-startup . better-jumper-mode)
  :init
  ;; Map extra mouse buttons to jump forward/backward
  (global-set-key [mouse-8] #'better-jumper-jump-backward)
  (global-set-key [mouse-9] #'better-jumper-jump-forward))

(use-package crux
  :straight t)


(provide 'me-extra)
