;;; me-tramp-containers.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn@cneebg.pbz")
;; Created: 2025-10-23
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:


;; TRAMP integration for Incus containers
(use-package incus-tramp
  :ensure t
  :after tramp
  :init
  (incus-tramp-add-method))


;; TRAMP integration for LXC containers
(use-package lxc-tramp
  :ensure t)


;; TRAMP integration for LXD containers
(use-package lxd-tramp
  :ensure t)


(provide 'obsolete/me-tramp-containers)
;;; me-tramp-containers.el ends here
