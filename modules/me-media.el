;; -*- lexical-binding: t; -*-

(defconst MPV-P (executable-find "mpv"))


(use-package empv
  :straight (:host github :repo "isamert/empv.el")
  :when MPV-P
  :general
  (+map 
    "ov"  '(nil :wk "empv")
    "ovp" '(empv-play :wk "Play")
    "ovy" '(consult-empv-youtube :wk "Seach Youtube")
    "ovr" '(empv-play-radio :wk "Play radio")
    "ovs" '(+empv-save-playtlist-to-file :wk "Save current playlist"))
  :config
  ;; See https://docs.invidious.io/instances/
  (setq empv-invidious-instance "https://invidious.projectsegfau.lt/api/v1"
        empv-audio-dir "~/Music"
        empv-video-dir "~/Videos"
        empv-max-directory-search-depth 6
        empv-radio-log-file (expand-file-name "logged-radio-songs.org" org-directory)
        empv-audio-file-extensions '("webm" "mp3" "ogg" "wav" "m4a" "flac" "aac" "opus")
        ;; Links from https://www.radio-browser.info
        empv-radio-channels
        '(("El-Bahdja FM" . "http://webradio.tda.dz:8001/ElBahdja_64K.mp3")
          ("El-Chaabia" . "https://radio-dzair.net/proxy/chaabia?mp=/stream")
          ("Quran Radio" . "http://stream.radiojar.com/0tpy1h0kxtzuv")
          ("Algeria International" . "https://webradio.tda.dz/Internationale_64K.mp3")
          ("JOW Radio" . "https://str0.creacast.com/jowradio")
          ("Europe1" . "http://ais-live.cloud-services.paris:8000/europe1.mp3")
          ("France Iter" . "http://direct.franceinter.fr/live/franceinter-hifi.aac")
          ("France Info" . "http://direct.franceinfo.fr/live/franceinfo-midfi.mp3")
          ("France Culture" . "http://icecast.radiofrance.fr/franceculture-hifi.aac")
          ("France Musique" . "http://icecast.radiofrance.fr/francemusique-hifi.aac")
          ("FIP" . "http://icecast.radiofrance.fr/fip-hifi.aac")
          ("Beur FM" . "http://broadcast.infomaniak.ch/beurfm-high.aac")
          ("Skyrock" . "http://icecast.skyrock.net/s/natio_mp3_128k")))

  (empv-playlist-loop-on)

  ;; Hacky palylist management (only supports saving playlist,
  ;; loading a playlist can be achieved using `empv-play-file')

  (defun +empv--dl-playlist (playlist &optional dist)
    (let ((default-directory
           (or dist
               (let ((d (expand-file-name "empv-downloads" empv-audio-dir)))
                 (unless (file-directory-p d) (mkdir d t)) d)))
          (vids (seq-filter
                 (apply-partially #'identity) ;; Filter nils
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


(provide 'me-spell)
