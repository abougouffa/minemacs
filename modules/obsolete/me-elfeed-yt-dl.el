;;; me-elfeed-yt-dl.el --- Extra commands for downloading videos using yt-dl -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(defcustom +elfeed-videos-dir "~/Videos/elfeed/"
  "Directory of downloaded videos."
  :group 'minemacs-elfeed)

(defcustom +yt-dlp-command "yt-dlp"
  "The \"yt-dlp\" command."
  :group 'minemacs-utils)

(defun +yt-dl-it (url)
  "Downloads the URL with \"yt-dlp\" in an async shell."
  (let ((default-directory (+directory-ensure +elfeed-videos-dir)))
    (async-shell-command (format "%s '%s'" +yt-dlp-command url))))

(defun +elfeed-youtube-dl (&optional use-generic-p)
  "Download Youtube videos."
  (interactive "P")
  (let ((entries (elfeed-search-selected)))
    (cl-loop for entry in entries
             do (elfeed-untag entry 'unread)
             when (elfeed-entry-link entry)
             do (+yt-dl-it it))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))


(provide 'obsolete/me-elfeed-yt-dl)
;;; me-elfeed-yt-dl.el ends here

