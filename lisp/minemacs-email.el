;; -*- lexical-binding: t; -*-

(defconst MU4E-LOAD-PATH "/usr/share/emacs/site-lisp/mu4e/")

(use-package mu4e
  :commands (mu4e)
  :when (file-directory-p MU4E-LOAD-PATH)
  :load-path MU4E-LOAD-PATH)

(use-package evil-mu4e
  :straight t
  :after mu4e)

(provide 'minemacs-email)
