;; -*- lexical-binding: t; -*-

(defconst MU4E-LOAD-PATH "/usr/share/emacs/site-lisp/mu4e/")

(use-package mu4e
  :defer t
  :when (file-directory-p MU4E-LOAD-PATH)
  :load-path MU4E-LOAD-PATH)

(provide 'minemacs-email)
