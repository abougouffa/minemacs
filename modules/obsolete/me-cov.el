;;; me-cov.el --- Code coverage -*- lexical-binding: t; -*-
;;
;; Filename: me-cov.el
;; Description:
;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")
;; Created: Sun Jul  9 23:26:43 2023 (+0200)
;; Version:
;; Last-Updated:
;;     Update #: 0
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(use-package cov
  :straight (:host github :repo "abougouffa/cov" :branch "feat/gcov-cmake")
  :custom
  (cov-highlight-lines t)
  :config
  (defun +cov-coverage-mode ()
    (interactive)
    (if cov-coverage-mode
        (progn
          (setq cov-coverage-mode nil)
          (message "Disabled coverage mode, showing how often lines are executed."))
      (setq cov-coverage-mode t)
      (message "Enabled coverage mode."))
    (cov-update)))


(provide 'obsolete/me-cov)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; me-cov.el ends here
