;;; me-project-cmake.el --- CMake integration for built-in project.el (replaced with projection) -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package project-cmake
  :straight (:host github :repo "juanjosegarciaripoll/project-cmake")
  :init
  (+map! :keymaps '(c-mode-map c++-mode-map c-ts-mode-map c++-ts-mode-map)
    :infix "p"
    "m" '(nil :wk "project-cmake")
    "mb" #'project-cmake-build
    "mg" #'project-cmake-configure
    "mt" #'project-cmake-test
    "mI" #'project-cmake-install
    "ms" #'project-cmake-scan-kits
    "mS" #'project-cmake-shell))


(provide 'obsolete/me-project-cmake)
;;; me-project-cmake.el ends here
