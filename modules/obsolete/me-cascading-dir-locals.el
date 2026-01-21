;;; me-cascading-dir-locals.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn@cneebg.pbz")
;; Created: 2026-01-21
;; Last modified: 2026-01-21

;;; Commentary:

;;; Code:


;; Apply all (!) ".dir-locals.el" from root to current directory
(use-package cascading-dir-locals
  :straight t
  :disabled
  :custom
  (cascading-dir-locals-debug minemacs-debug-p))


(provide 'obsolete/me-cascading-dir-locals)
;;; me-cascading-dir-locals.el ends here
