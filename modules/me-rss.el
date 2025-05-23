;;; me-rss.el --- News and RSS -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2022-10-05
;; Last modified: 2025-04-22

;;; Commentary:

;;; Code:

(defgroup minemacs-elfeed nil
  "MinEmacs elfeed tweaks."
  :group 'minemacs-apps)


;; An Emacs RSS web feeds client
(use-package elfeed
  :straight t
  :init
  (defcustom +elfeed-images-dir "~/Pictures/elfeed/"
    "Directory of downloaded pictures."
    :group 'minemacs-elfeed)
  (+def-dedicated-tab! elfeed :exit-func elfeed-search-quit-window)
  :bind (:map minemacs-open-thing-map ("f" . +elfeed-dedicated-tab))
  :custom
  (elfeed-db-directory (concat minemacs-local-dir "elfeed/db/"))
  (elfeed-enclosure-default-dir (concat minemacs-local-dir "elfeed/enclosure/"))
  :config
  ;; Hide the annoying index file form recent files
  (+ignore-root elfeed-db-directory elfeed-enclosure-default-dir)
  (add-to-list '+first-file-hook-ignore-list (concat elfeed-db-directory "index"))

  (defun +elfeed-download-image ()
    "Download the image at point."
    (interactive)
    (if-let* ((url (get-text-property (point) 'image-url)))
        (url-copy-file url (expand-file-name (url-file-nondirectory url) (+directory-ensure +elfeed-images-dir)))
      (user-error "No image at point!"))))


;; Extra `elfeed' protocols to add support for Fever, NewsBlur, Nextcloud/ownCloud News and Tiny Tiny RSS
(use-package elfeed-protocol
  :straight t
  :after elfeed
  :init
  (elfeed-protocol-enable))


(provide 'me-rss)

;;; me-rss.el ends here
