;;; me-lisp.el --- Deprecated Lisp module            -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(message "MinEmacs: The `me-lisp' module is obsolete. It has been divided to 4 modules: `me-emacs-lisp', `me-common-lisp', `me-clojure' and `me-scheme'")
(mapc (apply-partially #'+load minemacs-modules-dir) '("me-emacs-lisp.el" "me-common-lisp.el" "me-scheme.el" "me-clojure.el"))


(provide 'me-lisp)
;;; me-lisp.el ends here
