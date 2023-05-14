;;; me-rss.el --- News and RSS -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(use-package elfeed
  :straight t
  :init
  (+map! "of" #'elfeed)
  (+nmap! :keymaps 'elfeed-search-mode-map
    "<tab>" #'+elfeed-completing-filter
    "d" #'+elfeed-youtube-dl)
  (+nmap! :keymaps 'elfeed-show-mode-map
    "d" '+download-image)
  :custom
  (elfeed-db-directory (concat minemacs-local-dir "elfeed/db/"))
  (elfeed-enclosure-default-dir (concat minemacs-local-dir "elfeed/enclosure/"))
  :config
  ;; Hide the annoying index file form recent files
  (+ignore-root elfeed-db-directory elfeed-enclosure-default-dir))


(defun +download-image ()
  "Downloads the image under point"
  (interactive)
  (let ((url (get-text-property (point) 'image-url)))
    (if (not url)
        (message "No image under point!")
      (url-copy-file url (expand-file-name (url-file-nondirectory url)
                                           "~/Images/elfeed/")))))


(defun +yt-dl-it (url)
  "Downloads the URL in an async shell"
  (let ((default-directory "~/Vidéos"))
    (async-shell-command (format "yt-dlp '%s'" url))))

(defun +elfeed-youtube-dl (&optional use-generic-p)
  "Youtube-DL link"
  (interactive "P")
  (let ((entries (elfeed-search-selected)))
    (cl-loop for entry in entries
             do (elfeed-untag entry 'unread)
             when (elfeed-entry-link entry)
             do (+yt-dl-it it))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))


(provide 'me-rss)

;;; me-rss.el ends here
