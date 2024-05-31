;;; me-ffip.el --- Projects stuff -*- lexical-binding: t; -*-

(use-package find-file-in-project
  :straight t
  :custom
  (ffip-use-rust-fd (and (executable-find "fd") t)))

(provide 'obsolete/me-ffip)
