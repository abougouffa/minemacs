;;; me-bitwarden.el --- Bitwarden password manager integration into Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package bitwarden
  :straight (:host github :repo "seanfarley/emacs-bitwarden")
  :when (executable-find "bw")
  :custom
  (bitwarden-automatic-unlock
   (lambda ()
     (require 'auth-source)
     (if-let* ((matches (auth-source-search :host "bitwarden.com" :max 1))
               (entry (nth 0 matches))
               (email (plist-get entry :user))
               (pass (plist-get entry :secret)))
         (progn (setq bitwarden-user email)
                (if (functionp pass) (funcall pass) pass))
       ""))))


(provide 'obsolete/me-bitwarden)
;;; me-bitwarden.el ends here
