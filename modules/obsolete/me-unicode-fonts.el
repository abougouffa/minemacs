;;; me-unicode-fonts.el --- Unicode fonts            -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2023-07-18
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:

(use-package pcache
  :straight t
  :custom
  (pcache-directory (concat minemacs-cache-dir "pcache/")))

(use-package unicode-fonts
  :straight t
  :hook (minemacs-lazy . +unicode-fonts-setup)
  :init
  (defun +unicode-fonts-setup ()
    "Prefer the `:unicode-font-family' from `minemacs-fonts'."
    (interactive)
    (when-let* ((frame (selected-frame)))
      (when (display-multi-font-p frame)
        (with-selected-frame frame
          (when-let* ((unicode-font-family (plist-get minemacs-fonts :unicode-font-family)))
            (dolist (unicode-block unicode-fonts-block-font-mapping)
              (push unicode-font-family (cadr unicode-block))))
          (unicode-fonts-setup))))))


(provide 'obsolete/me-unicode-fonts)
;;; me-unicode-fonts.el ends here
