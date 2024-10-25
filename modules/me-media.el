;;; me-media.el --- Multimedia stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(defcustom +mpv-command "mpv"
  "The MPV command."
  :group 'minemacs-utils
  :type 'string)


(when (executable-find +mpv-command)
  (defun +browse-url-mpv (url &optional _args)
    "Open URL with MPV."
    (start-process "browse-url:mpv" " *MPV:browse-url*" +mpv-command url))
  ;; Automatically open Youtube links in MPV
  (setq browse-url-browser-function
        `((,(rx (seq "http" (? ?s) "://" (? "www.") (or "youtube.com" "youtu.be"))) . +browse-url-mpv)
          ("." . ,browse-url-browser-function))))


;; An Emacs media player, media library manager, radio player, YouTube frontend
(use-package empv
  :straight (:host github :repo "isamert/empv.el")
  :when (executable-find +mpv-command)
  :custom
  (empv-invidious-instance "https://invidious.privacydev.net/api/v1") ; Pick from: https://api.invidious.io
  (empv-radio-log-file (concat org-directory "logged-radio-songs.org"))
  (empv-audio-file-extensions '("webm" "mp3" "ogg" "wav" "m4a" "flac" "aac" "opus"))
  :config
  (defun +empv--dl-playlist (playlist &optional dist)
    (let ((proc-name "empv-yt-dlp")
          (default-directory (or dist (let ((dir (expand-file-name "empv-downloads" empv-audio-dir)))
                                        (unless (file-directory-p dir) (mkdir dir t)) dir)))
          (vids (seq-filter
                 #'identity ; Filter nils
                 (mapcar
                  (lambda (item) ; Extract ID from URL patterns youtube.com/watch?v=8x7eUKYhBKg or youtu.be/8x7eUKYhBKg
                    (and (string-match (rx (seq (or "watch?v=" "youtu.be/") (group-n 1 (* (any alnum "_" "-"))))) item)
                         (match-string 1 item)))
                  playlist))))
      (when (length> vids 0)
        (message "Downloading %d songs to %s" (length vids) default-directory)
        (when (get-process proc-name) (kill-process proc-name))
        (make-process
         :name proc-name
         :buffer (format "*%s*" proc-name)
         :command `("yt-dlp" "--no-abort-on-error" "--no-colors" "--no-progress" "--extract-audio" "-f" "bestaudio" ,@vids)
         :sentinel (lambda (_proc event)
                     (when (string= event "finished\n")
                       (message "Finished downloading playlist files!")))))))

  (defun +empv-download-playtlist-files (&optional path)
    (interactive "DSave download playlist files to: ")
    (empv--playlist-apply #'+empv--dl-playlist path)))


;; An Emacs major mode to open media (audio/video) files like any other file (via `find-file', `dired', etc)
(use-package ready-player
  :straight (:host github :repo "xenodium/ready-player")
  :hook (minemacs-first-file . ready-player-mode))


(provide 'me-media)

;;; me-media.el ends here
