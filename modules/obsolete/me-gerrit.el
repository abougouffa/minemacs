;;; me-gerrit.el --- Gerrit stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-06-11
;; Last modified: 2025-10-08

;;; Commentary:

;;; Code:


;; The Emacs Gerrit Experience
(use-package gee
  :straight (:host bitbucket :repo "olanilsson/gee")
  :commands (gee-gerrit-dashboard-self)
  :config
  (advice-add
   'gee--prest-request :around
   (satch-defun +gee--basic-http-auth (orig-fn &rest args)
     (let ((url-request-extra-headers (when-let* ((token (+gee--get-basic-http-authentication-token)))
                                        `(("Authorization" . ,token)))))
       (apply orig-fn args))))

  (defun +gee--get-basic-http-authentication-token ()
    (when-let* ((auth (car (auth-source-search :host (url-host (url-generic-parse-url gee-gerrit-rest-base)))))
                (user (plist-get auth :user))
                (token (plist-get auth :secret)))
      (concat "Basic " (base64-encode-string (concat user ":" (funcall token)))))))


;; Gerrit integration via the REST API
(use-package gerrit
  :straight (:host github :repo "twmr/gerrit.el"))


;; Transient menus to use some "repo" commands within Magit
(use-package repo-transient
  :straight (chromiumos-dev-utils :type git :repo "https://chromium.googlesource.com/chromiumos/platform/dev-util" :files ("contrib/emacs/gerrit/*"))
  :commands (repo-main-menu))


(provide 'obsolete/me-gerrit)
;;; me-gerrit.el ends here
