;;; me-tributary.el --- Confluence integration -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn@cneebg.pbz")
;; Created: 2025-05-16
;; Last modified: 2025-05-16

;;; Commentary:

;;; Code:


;; Edit Confluence wiki pages in Emacs
(use-package tributary
  :straight (:host github :repo "mrkrd/tributary" :files (:defaults "confluence.rnc"))
  :commands (tributary-mode tributary-push tributary-pull-id tributary-pull-url))


(provide 'obsolete/me-tributary)
;;; me-tributary.el ends here
