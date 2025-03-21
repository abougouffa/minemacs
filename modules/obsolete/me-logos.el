;;; me-logos.el --- Focus and narrow -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package logos
  :straight t
  :custom
  ;; If you want to use outlines instead of page breaks (the ^L):
  (logos-outlines-are-pages t)
  ;; This is the default value for the outlines:
  (logos-outline-regexp-alist `((emacs-lisp-mode . "^;;;+ ")
                                (org-mode . "^\\*+ +")
                                (markdown-mode . "^\\#+ +")))
  ;; These apply when `logos-focus-mode' is enabled.  Their value is buffer-local.
  (logos-hide-cursor nil)
  (logos-hide-mode-line t)
  (logos-hide-header-line t)
  (logos-hide-buffer-boundaries t)
  (logos-hide-fringe t)
  (logos-variable-pitch nil)
  (logos-buffer-read-only nil)
  (logos-scroll-lock nil)
  :init
  (let ((map global-map))
    (define-key map [remap narrow-to-region] #'logos-narrow-dwim)
    (define-key map [remap forward-page] #'logos-forward-page-dwim)
    (define-key map [remap backward-page] #'logos-backward-page-dwim)
    (define-key map (kbd "<f9>") #'logos-focus-mode)))


(provide 'obsolete/me-logos)
;;; me-logos.el ends here
