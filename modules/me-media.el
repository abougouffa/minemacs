;;; me-media.el --- Multimedia stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2022-10-20
;; Last modified: 2026-09-25

;;; Commentary:

;;; Code:

(defcustom +mpv-command "mpv"
  "The MPV command."
  :group 'minemacs-utils
  :type 'string)


;; Automatically open YouTube links in MPV
(with-eval-after-load 'browse-url
  (when (executable-find +mpv-command)
    (defun +browse-url-mpv (url &optional _args)
      "Open URL with MPV."
      (start-process "browse-url:mpv" " *MPV:browse-url*" +mpv-command url)))
  (setq browse-url-handlers
        `((,(rx (seq "http" (? ?s) "://" (? "www.") (or "youtube.com" "youtu.be"))) . +browse-url-mpv)
          ("." . ,browse-url-browser-function))))


;; An Emacs media player, media library manager, radio player, YouTube frontend
(use-package empv
  :straight (:host github :repo "isamert/empv.el")
  :when (executable-find +mpv-command)
  :custom
  (empv-radio-log-file (concat org-directory "logged-radio-songs.org"))
  (empv-invidious-instance 'ivjs)
  (empv-audio-file-extensions '("webm" "mp3" "ogg" "wav" "m4a" "flac" "aac" "opus"))
  :config
  ;; BUGFIX: Ensure adding the trailing "/" to directories
  (dolist (dir '(empv-audio-dir empv-video-dir empv-playlist-dir))
    (set dir (file-name-as-directory (symbol-value dir))))

  (defun +empv--dl-playlist (playlist)
    (when-let* ((yt-vids (seq-filter (lambda (item) ; Extract Youtube videos
                                       (and (string-match (rx (seq (or "watch?v=" "youtu.be/") (group-n 1 (* (any alnum "_" "-"))))) item)
                                            item))
                                     playlist)))
      (mapc (lambda (link) (empv-youtube-download link nil (lambda (where) (+log! "Successfully downloaded %s to %s" link where)))) yt-vids)))

  (defun +empv-download-playtlist-files ()
    (interactive)
    (empv--playlist-apply #'+empv--dl-playlist)))


;; An Emacs major mode to open media (audio/video) files like any other file (via `find-file', `dired', etc)
(use-package ready-player
  :straight (:host github :repo "xenodium/ready-player" :files (:defaults "*.el"))
  :after minemacs-first-file
  :demand
  :custom
  (ready-player-minor-mode-map-prefix "C-c o p")
  (ready-player-ask-for-project-sustainability nil)
  :config
  ;; Enable only when we have at least one supported media player installed
  (when (and nil ; BUG: Disabled for now because of this bug: xenodium/ready-player#24
             (seq-some #'executable-find (mapcar #'car ready-player-open-playback-commands)))
    (ready-player-mode 1)))


(provide 'me-media)

;;; me-media.el ends here
