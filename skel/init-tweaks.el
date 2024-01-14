;;; init-tweaks.el --- Initialization tweaks, loaded early in the Emacs' "init.el" -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(with-eval-after-load 'me-lib
  ;; Define special "first file" hooks and features for Python and C/C++ files,
  ;; this will creates the hooks `minemacs-first-c/c++-file-hook' and and
  ;; `minemacs-first-python-file-hook' that will provide the features
  ;; `minemacs-first-c/c++-file' and `minemacs-first-python-file'.
  (+make-first-file-hook! 'c/c++ (rx "." (or "c" "cpp" "cxx" "cc" "c++" "h" "hpp" "hxx" "hh" "h++" "ixx" "cppm" "cxxm" "c++m" "ccm") eol))
  (+make-first-file-hook! 'python (rx "." (or "py" "pyw" "pyx" "pyz" "pyzw") eol)))


;;; init-tweaks.el ends here
