;;; me-cmake-build.el --- CMake build -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;; CMake building with multiple targets, run configurations and interactive menu
(use-package cmake-build
  :straight (:host github :repo "ultronozm/cmake-build.el")
  :preface
  ;; For some reason, Emacs keeps creating a copy of the file under "~/.emacs.d/" even when the package isn't loaded!
  (setq cmake-build-local-options-file (concat minemacs-local-dir "cmake-build-options.el"))
  :commands (cmake-build-clean
             cmake-build-clear-cache-and-configure
             cmake-build-current
             cmake-build-debug
             cmake-build-delete-current-windows
             cmake-build-other-target
             cmake-build-run
             cmake-build-run-cmake
             cmake-build-set-buffer-local-config
             cmake-build-set-cmake-profile
             cmake-build-set-config
             cmake-build-set-options
             cmake-build-set-project-build-root
             cmake-build-set-project-root))


;; Helper commands and functions for working with C++ projects
(use-package czm-cpp
  :straight (:host github :repo "ultronozm/czm-cpp.el"))


(provide 'obsolete/me-cmake-build)
;;; me-cmake-build.el ends here
