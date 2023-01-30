;;; me-lifestyle.el --- *Highly* opinionated lifestyle apps -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


(use-package awqat
  :straight (:host github :repo "zkry/awqat")
  :commands awqat-times-for-day
  :custom
  (awqat-mode-line-format " 🕌 ${prayer} (${hours}h${minutes}m) ")
  (awqat-update-interval 30.0))


(provide 'me-lifestyle)
