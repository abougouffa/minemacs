;;; me-djvu.el --- Déjà vu (DjVu) documents -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-djvu
  :auto-mode '(("\\.[dD][jJ][vV][uU]?\\'" . djvu-init-mode))
  :magic-mode '(("%DJVU" . djvu-read-mode)))

(use-package djvu
  :straight t
  :magic ("%DJVU" . djvu-read-mode)
  :config
  (defun +djvu-toggle-continuous-scrolling ()
    (interactive)
    (setq djvu-continuous (not djvu-continuous))
    (message "Djvu alternative scrolling %s" (if djvu-continuous "enabled" "disabled"))))

(use-package djvu3
  :straight (:host github :repo "dalanicolai/djvu3")
  :after djvu
  :demand
  :init
  (+setq-hook! djvu-read-mode
    imenu-create-index-function #'djvu-imenu-create-index
    imenu-default-goto-function (lambda (title page) (djvu-goto-page page djvu-doc)))
  :config
  ;; `djvu-continuous' of `djvu' does not work with `djvu3', lets make emulate it
  (defun +djvu-scroll-up-or-next-page ()
    (interactive)
    (if (not djvu-continuous)
        (if djvu-image-mode (djvu-image-scroll-up) (next-line))
      (scroll-up-line 5)
      (when (= (window-vscroll) 0)
        (djvu-next-page 1))))

  (defun +djvu-scroll-down-or-previous-page ()
    (interactive)
    (if (not djvu-continuous)
        (if djvu-image-mode (djvu-image-scroll-down) (previous-line))
      (if (/= (window-vscroll) 0)
          (scroll-down-line 5)
        (djvu-prev-page 1)
        (scroll-up-command))))

  (defun +djvu-fast-search (regexp)
    (interactive "sSearch (regexp): ")
    (when djvu-image-mode
      (djvu-image-toggle))
    (+djvu-search-forward regexp))

  (defun +djvu-search-forward (query)
    "Search forward for match for REGEXP.
Search case-sensitivity is determined by the value of the variable
`case-fold-search', see Info node `(elisp)Searching and Case'.
Use the command `+djvu-search-forward-continue' to continue the search."
    (interactive "sQuery: ")
    (setq djvu-last-search-re query)
    (unless (eq (djvu-ref page) (djvu-ref pagemax))
      (search-forward query nil t))
    (djvu-goto-page (let ((page (djvu-ref page))
                          (return 1))
                      (while (and (/= return 0) (< page (+ (djvu-ref pagemax) 1)))
                        (setq page (1+ page))
                        (setq return (call-process-shell-command
                                      (format "djvused %s -e 'select %s; print-pure-txt' | grep -i '%s'"
                                              (shell-quote-argument buffer-file-name)
                                              page
                                              query))))
                      page))
    (search-forward query nil t))

  (defun +djvu-search-forward-continue ()
    "Continue search forward for match for `djvu-last-search-re'."
    (interactive)
    (+djvu-search-forward djvu-last-search-re)))


(provide 'on-demand/me-djvu)
;;; me-djvu.el ends here
