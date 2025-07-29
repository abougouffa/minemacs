;;; me-spdx.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn@cneebg.pbz")
;; Created: 2025-07-29
;; Last modified: 2025-07-29

;;; Commentary:

;;; Code:


;; Insert SPDX license header
(use-package spdx
  :straight (:host github :repo "condy0919/spdx.el")
  :custom
  (spdx-copyright-holder 'user)
  (spdx-project-detection 'auto))


(provide 'obsolete/me-spdx)
;;; me-spdx.el ends here
