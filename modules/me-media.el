;;; me-media.el --- Multimedia stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2022-10-20
;; Last modified: 2026-08-07

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

  (defun +empv-pick-individous-instance ()
    "Pick an Individous instance with API support from https://api.invidious.io."
    (when-let* ((instances (with-current-buffer
                               (url-retrieve-synchronously "https://api.invidious.io/instances.json?sort_by=api,type,users")
                             (goto-char url-http-end-of-headers)
                             (let ((json-key-type 'symbol)
                                   (json-array-type 'list)
                                   (json-object-type 'alist))
                               (json-read)))))
      (if-let* ((instance (cadr (seq-find
                                 (lambda (instance)
                                   (let ((opts (cadr instance)))
                                     (and
                                      (string-match-p "^https?$" (alist-get 'type opts)) ; https
                                      (not (eq (alist-get 'api opts) json-false))))) ; and has API support
                                 instances))))
          (setopt empv-invidious-instance (concat (alist-get 'uri instance) "/api/v1"))
        (message "There is no available Invidious instance with API support."))))

  (defun +empv--dl-playlist (playlist)
    (when-let* ((yt-vids (seq-filter (lambda (item) ; Extract Youtube videos
                                       (and (string-match (rx (seq (or "watch?v=" "youtu.be/") (group-n 1 (* (any alnum "_" "-"))))) item)
                                            item))
                                     playlist)))
      (mapcar (lambda (link) (empv-youtube-download link nil (lambda (where) (+log! "Successfully downloaded %s to %s" link where)))) yt-vids)))

  (defun +empv-download-playtlist-files ()
    (interactive)
    (empv--playlist-apply #'+empv--dl-playlist)))


(provide 'me-media)

;;; me-media.el ends here
