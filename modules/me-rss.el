;;; me-rss.el --- News and RSS -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(defgroup minemacs-elfeed nil
  "MinEmacs elfeed tweaks."
  :group 'minemacs)

(use-package elfeed
  :straight t
  :init
  (+map! "of" #'elfeed)
  (+nmap! :keymaps 'elfeed-search-mode-map
    "<tab>" #'+elfeed-completing-filter
    "d" #'+elfeed-youtube-dl)
  (+nmap! :keymaps 'elfeed-show-mode-map
    "D" #'+elfeed-download-image)
  (defcustom +elfeed-videos-dir "~/Videos/elfeed/"
    "Directory of downloaded videos."
    :group 'minemacs-elfeed)
  (defcustom +elfeed-images-dir "~/Pictures/elfeed/"
    "Directory of downloaded pictures."
    :group 'minemacs-elfeed)
  (defcustom +yt-dlp-command "yt-dlp"
    "The \"yt-dlp\" command."
    :group 'minemacs-tools)
  (defvar +elfeed-file "elfeed.org"
   "Name of org file.")
  :custom
  (elfeed-db-directory (concat minemacs-local-dir "elfeed/db/"))
  (elfeed-enclosure-default-dir (concat minemacs-local-dir "elfeed/enclosure/"))
  :config
  ;; Hide the annoying index file form recent files
  (+ignore-root elfeed-db-directory elfeed-enclosure-default-dir)

  (defun +elfeed-download-image ()
    "Download the image at point."
    (interactive)
    (let ((url (get-text-property (point) 'image-url)))
      (if (not url)
          (message "No image at point!")
        (url-copy-file
         url (expand-file-name (url-file-nondirectory url)
                               (+directory-ensure +elfeed-images-dir))))))

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

  (defun +elfeed-filter-results-count (search-filter)
   "Count results for SEARCH-FILTER."
   (let* ((filter (elfeed-search-parse-filter search-filter))
          (head (list nil))
          (tail head)
          (count 0))
     (let ((lexical-binding t)
           (func (byte-compile (elfeed-search-compile-filter filter))))
       (with-elfeed-db-visit (entry feed)
         (when (funcall func entry feed count)
           (setf (cdr tail) (list entry)
                 tail (cdr tail)
                 count (1+ count)))))
     count))

  (defun +elfeed-view-filtered (filter)
    "Filter the elfeed-search buffer to show feeds tagged with FILTER."
    (interactive)
    (elfeed)
    (unwind-protect
        (let ((elfeed-search-filter-active :live))
          (setq elfeed-search-filter filter))
      (elfeed-search-update :force)))

  (defun +elfeed-get-tags ()
   (let ((all-tags '()))
     (with-temp-buffer (insert-file-contents (concat minemacs-config-dir +elfeed-file)
                                             (delay-mode-hooks (org-mode)))
        (org-map-entries
         (lambda ()
           (let ((tag-string (car (last (org-heading-components)))))
             (when tag-string
               (setq all-tags
                     (append all-tags (split-string tag-string ":" t)))))))

      ;; now get counts
      (loop for tag in (seq-uniq all-tags)
            collect  (cons tag (format "@6months-ago +unread +%s" tag))))))

 (defun +elfeed-completing-filter ()
   "Completing filter."
   (interactive)
   (let* ((tags (append '(("All" . "@6-months-ago +unread")) (+elfeed-get-tags) nil))
          (categories (-filter
                       (lambda (item)
                         (> (+elfeed-filter-results-count (cdr item))
                            0))
                       tags)))

     (if ( > (length categories) 0)
         (progn
           (+elfeed-view-filtered (cdr (assoc (completing-read "Categories: " categories)
                                              categories)))
           (goto-char (window-start)))
       (message "All caught up \\o/")))))

(use-package elfeed-org
  :straight t
  :after (elfeed)
  :init
  (setq rmh-elfeed-org-files (list (concat minemacs-config-dir +elfeed-file)))
  (elfeed-org))


(provide 'me-rss)

;;; me-rss.el ends here
