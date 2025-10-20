;;; me-xref-union.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn@cneebg.pbz")
;; Created: 2025-10-20
;; Last modified: 2025-10-20

;;; Commentary:

;;; Code:


;; Combine multiple Xref backends
(use-package xref-union
  :straight t
  :commands (xref-union-mode)
  :custom
  ;; BUG+HACK: When in `xref-union-mode', the `xref-union--backend' seems to
  ;; access all the backends, including the ones that aren't enabled locally.
  ;; The list includes `etags' which is annoying since it asks about the TAGS
  ;; file, which interrupts `xref-union' from trying the rest of the backends.
  ;; So, lets exclude `etags' since I'm not using it, the predicate function can
  ;; be modified to check for other conditions (for example: enable `etags' in
  ;; some circumstances)
  (xref-union-excluded-backends #'+xref-union--exclude-backends-predicate)
  :config
  (defun +xref-union--exclude-backends-predicate (backend)
    (memq backend '(etags--xref-backend))))


(provide 'obsolete/me-xref-union)
;;; me-xref-union.el ends here
