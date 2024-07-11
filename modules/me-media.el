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

(use-package empv
  :straight (:host github :repo "isamert/empv.el")
  :when (executable-find +mpv-command)
  :custom
  ;; See: docs.invidious.io/instances/
  (empv-invidious-instance "https://invidious.projectsegfau.lt/api/v1")
  (empv-audio-dir "~/Music")
  (empv-video-dir "~/Videos")
  (empv-max-directory-search-depth 6)
  (empv-radio-log-file (expand-file-name "logged-radio-songs.org" org-directory))
  (empv-audio-file-extensions '("webm" "mp3" "ogg" "wav" "m4a" "flac" "aac" "opus"))
  :config
  (defun +empv--dl-playlist (playlist &optional dist)
    (let ((default-directory
           (or dist
               (let ((d (expand-file-name "empv-downloads" empv-audio-dir)))
                 (unless (file-directory-p d) (mkdir d t)) d)))
          (vids (seq-filter
                 #'identity ;; Filter nils
                 (mapcar
                  (lambda (item)
                    (when-let
                        ((vid (when (string-match
                                     (rx (seq "watch?v=" (group-n 1 (one-or-more (or alnum "_" "-")))))
                                     item)
                                (match-string 1 item))))
                      vid))
                  playlist)))
          (proc-name "empv-yt-dlp"))
      (unless (zerop (length vids))
        (message "Downloading %d songs to %s" (length vids) default-directory)
        (when (get-process proc-name)
          (kill-process proc-name))
        (make-process :name proc-name
                      :buffer (format "*%s*" proc-name)
                      :command (append
                                (list
                                 (executable-find "yt-dlp")
                                 "--no-abort-on-error"
                                 "--no-colors"
                                 "--extract-audio"
                                 "--no-progress"
                                 "-f" "bestaudio")
                                vids)
                      :sentinel (lambda (prc event)
                                  (when (string= event "finished\n")
                                    (message "Finished downloading playlist files!")))))))

  (defun +empv-download-playtlist-files (&optional path)
    (interactive "DSave download playlist files to: ")
    (empv--playlist-apply #'+empv--dl-playlist path)))

(use-package emms
  :straight t)

(use-package ready-player
  :straight (:host github :repo "xenodium/ready-player")
  :hook (minemacs-first-file . ready-player-mode))


(provide 'me-media)

;;; me-media.el ends here
