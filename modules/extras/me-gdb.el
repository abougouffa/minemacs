;;; me-gdb.el --- Extra tweaks for GDB, and opt-in emacs-gdb integration -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:


;;;###autoload
(defun +emacs-gdb-enable ()
  "Load a faster \"gdb\" command from \"emacs-gdb\".
This will overwrite the built-in \"gdb-mi\" for this session."
  (interactive)
  (if (+emacs-features-p 'modules)
      (when (y-or-n-p "Loading \"emacs-gdb\" will overwrite \"gdb-mi\" for this session, continue?")
        (use-package gdb-mi
          ;; I use my own fork in which I've merged some open PRs on the upstream.
          :straight `(:host github
                      :repo "weirdNox/emacs-gdb" :files (:defaults "*.c" "*.h" "Makefile")
                      :fork (:repo "abougouffa/emacs-gdb"))
          :demand
          :init
          (fmakunbound 'gdb)
          (fmakunbound 'gdb-enable-debug)
          :custom
          (gdb-window-setup-function #'gdb--setup-windows)
          (gdb-ignore-gdbinit nil)))
    (user-error "Cannot enable \"emacs-gdb\", Emacs was built without modules support!")))


(provide 'me-gdb)

;;; me-gdb.el ends here
