;;; me-devicetree.el --- Devicetree support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-10
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-devicetree
  :auto-mode '(("\\.dtsi?\\'" . dts-mode)
               (".+\\.dtb\\|dtbo\\'" . virtual-dts-mode)))


;; Major mode for DeviceTree source code
(use-package dts-mode
  :ensure t)


;; Major mode for Device Tree Binary (`*.dtb') files
(use-package virtual-dts-mode
  :vc (:url "https://github.com/connorfeeley/virtual-dts-mode"))


(provide 'on-demand/me-devicetree)
;;; me-devicetree.el ends here
