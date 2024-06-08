;;; me-spdx.el --- Insert SPDX licences IDs -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package spdx
  :straight (:host github :repo "condy0919/spdx.el")
  :custom
  (spdx-copyright-holder 'user)
  (spdx-project-detection 'auto))


(provide 'obsolete/me-spdx)
;;; me-spdx.el ends here
