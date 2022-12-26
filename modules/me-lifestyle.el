;; -*- lexical-binding: t; -*-


(use-package awqat
  :straight (:host github :repo "zkry/awqat")
  :hook (minemacs-after-startup . awqat-display-prayer-time-mode)
  :commands awqat-times-for-day
  :custom
  (awqat-mode-line-format " 🕌 ${prayer} (${hours}h${minutes}m) ")
  (awqat-update-interval 30.0)
  :config
  ;; Make sure `calendar-latitude' and `calendar-longitude' are set,
  ;; otherwise, set them here.
  (awqat-set-preset-french-muslims))


(provide 'me-lifestyle)
