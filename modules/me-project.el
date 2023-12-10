;;; me-project.el --- Projects stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa and contributors

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(use-package consult-project-extra
  :straight t
  :init
  (+map! :infix "p" ;; project
    "p" #'consult-project-extra-find
    "P" #'consult-project-extra-find-other-window))

(use-package ibuffer-project
  :straight t
  :hook (ibuffer . +ibuffer-project-h)
  :config
  ;; From Crafted Emacs
  (defun +ibuffer-project-h ()
    (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
    (unless (eq ibuffer-sorting-mode 'project-file-relative)
      (ibuffer-do-sort-by-project-file-relative))))

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


(provide 'me-project)

;;; me-project.el ends here
