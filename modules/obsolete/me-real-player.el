;;; me-real-player.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn@cneebg.pbz")
;; Created: 2026-04-27
;; Last modified: 2026-04-27

;;; Commentary:

;; Made obsolete because of this annoying bug: xenodium/ready-player#24

;;; Code:


;; An Emacs major mode to open media (audio/video) files like any other file (via `find-file', `dired', etc)
(use-package ready-player
  :straight (:host github :repo "xenodium/ready-player" :files (:defaults "*.el"))
  :after minemacs-first-file
  :demand
  :custom
  (ready-player-minor-mode-map-prefix "C-c o p")
  :config
  ;; Enable only when we have at least one supported media player installed
  (when (seq-some #'executable-find (mapcar #'car ready-player-open-playback-commands))
    (ready-player-mode 1)))


(provide 'obsolete/me-real-player)
;;; me-real-player.el ends here
