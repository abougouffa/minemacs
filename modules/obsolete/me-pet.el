;;; me-pet.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn@cneebg.pbz")
;; Created: 2025-07-21
;; Last modified: 2025-07-21

;;; Commentary:

;;; Code:


;; Python Executable Tracker
(use-package pet
  :straight t
  :when (and (or (executable-find "dasel") (executable-find "yq"))
             (or (featurep 'feat/sqlite3) (executable-find "sqlite3")))
  :init
  ;; BUG: When accessing files via ADB, `pet-mode' fails at some stage because
  ;; `tramp' isn't able to give a relavant information in
  ;; `tramp-handle-file-directory-p'. After tracing this down, it seems like
  ;; `file-attributes' doesn't support my "adb" for now.
  (defun +pet-mode-maybe ()
    (when-let* ((path (or (buffer-file-name (or (buffer-base-buffer) (current-buffer))) default-directory))
                ((or (not (equal "adb" (file-remote-p path 'method)))
                     (file-attributes path))))
      (pet-mode 1)))

  ;; TODO: Try to find a better way of applying `pet-mode', currently, it slows
  ;; down opening Python buffers (or reverting them)
  (add-hook 'python-base-mode-hook '+pet-mode-maybe -10))


(provide 'obsolete/me-pet)
;;; me-pet.el ends here
