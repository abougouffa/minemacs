;;; me-drag-stuff.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn@cneebg.pbz")
;; Created: 2025-08-13
;; Last modified: 2025-08-13

;;; Commentary:

;;; Code:


;; Drag stuff around in Emacs
(use-package drag-stuff
  :straight t
  :init
  :bind (("M-<up>" . drag-stuff-up)
         ("M-<down>" . drag-stuff-down)
         ("M-<left>" . drag-stuff-left)
         ("M-<right>" . drag-stuff-right)))


(provide 'obsolete/me-drag-stuff)
;;; me-drag-stuff.el ends here
