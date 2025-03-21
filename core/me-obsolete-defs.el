;; me-obsolete-defs.el -- Obsolete definitions -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(require 'me-lib)

(defconst os/android (+emacs-options-p 'os/android) "Non-nil on Android systems.")
(defconst os/linux (+emacs-options-p 'os/linux) "Non-nil on GNU/Linux systems.")
(defconst os/bsd (+emacs-options-p 'os/bsd) "Non-nil on BSD systems.")
(defconst os/win (+emacs-options-p 'os/win) "Non-nil on Windows systems.")
(defconst os/mac (+emacs-options-p 'os/mac) "Non-nil on MacOS systems.")
(defconst sys/arch (intern (car (split-string system-configuration "-")))
  "The system's architecture read from `system-configuration'.
It return a symbol like `x86_64', `aarch64', `armhf', ...")

(make-obsolete-variable 'os/android '+emacs-options-p "v11.0.0")
(make-obsolete-variable 'os/linux '+emacs-options-p "v11.0.0")
(make-obsolete-variable 'os/bsd '+emacs-options-p "v11.0.0")
(make-obsolete-variable 'os/win '+emacs-options-p "v11.0.0")
(make-obsolete-variable 'os/mac '+emacs-options-p "v11.0.0")
(make-obsolete-variable 'sys/arch '+emacs-options-p "v11.0.0")

(define-obsolete-function-alias '+emacs-features-p '+emacs-options-p "v11.0.0")


(provide 'me-obsolete-defs)
;;; me-obsolete-defs.el ends here
