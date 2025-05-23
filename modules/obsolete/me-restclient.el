;;; me-restclient.el --- Restclient -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-01
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:


(use-package restclient
  :straight (:host github :repo "pashky/restclient.el" :files ("*.el") :fork (:repo "abougouffa/restclient.el"))
  :hook (restclient-mode . +prog-mode-run-hooks)
  :mode ("\\.http\\'" . restclient-mode)
  :config
  (require 'restclient-jq)
  (+setq-hook! restclient-mode
    imenu-generic-expression '((nil "^[A-Z]+\s+.+" 0)))

  ;; From Doom Emacs (in case `gnutls-verify-error' policy is set to something)
  (advice-add
   #'restclient-http-do :around
   (satch-defun +restclient--permit-self-signed-ssl:around-a (orig-fn &rest args)
     "Forces underlying SSL verification to prompt for self-signed or invalid
certs, rather than reject them silently."
     (require 'gnutls)
     (let (gnutls-verify-error) (apply orig-fn args)))))

(use-package restclient-test
  :straight t)

(use-package ob-restclient
  :straight t
  :after org
  :init
  (org-babel-do-load-languages 'org-babel-load-languages '((restclient . t))))


(provide 'obsolete/me-restclient)
;;; me-restclient.el ends here
