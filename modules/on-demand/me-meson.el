;;; me-meson.el --- Meson -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn@cneebg.pbz")
;; Created: 2026-02-04
;; Last modified: 2026-02-04

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-meson
  :auto-mode '(("/meson\\(\\.build\\|_options\\.txt\\|\\.options\\)\\'" . meson-mode)))


(use-package meson-mode
  :straight t)


(provide 'on-demand/me-meson)
;;; me-meson.el ends here
