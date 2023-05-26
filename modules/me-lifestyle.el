;;; me-lifestyle.el --- *Highly* opinionated lifestyle apps -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(use-package awqat
  :straight (:host github :repo "zkry/awqat")
  :commands awqat-times-for-day
  :custom
  ;; In your "config.el", you need to set `calendar-latitude' and
  ;; `calendar-longitude' (both defined in `solar'). Alongside with the
  ;; calculation method, either by setting the right angles for Fajr and Isha,
  ;; or by using one of the predefined presets (see `awqat' for more
  ;; information).
  (awqat-mode-line-format (concat " " (nerd-icons-mdicon "nf-md-mosque") " ${prayer} (${hours}h${minutes}m) "))
  (awqat-update-interval 30.0))


(provide 'me-lifestyle)

;;; me-lifestyle.el ends here
