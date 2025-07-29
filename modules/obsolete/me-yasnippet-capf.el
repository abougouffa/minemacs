;;; me-yasnippet-capf.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn@cneebg.pbz")
;; Created: 2025-07-29
;; Last modified: 2025-07-29

;;; Commentary:

;;; Code:


;; Completion-At-Point Extension for YASnippet
(use-package yasnippet-capf
  :straight t
  :bind (("C-c p y" . yasnippet-capf))
  :init
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))


(provide 'obsolete/me-yasnippet-capf)
;;; me-yasnippet-capf.el ends here
