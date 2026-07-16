;;; me-devicetree.el --- Devicetree support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-10
;; Last modified: 2026-07-16

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-devicetree
  :auto-mode '(("\\.dtsi?\\'" . devicetree-ts-mode)
               (".+\\.dtb\\|dtbo\\'" . virtual-dts-mode)))


;; Major mode for DeviceTree source code
(use-package dts-mode
  :straight t)


;; Tree-sitter support for DeviceTree
(use-package devicetree-ts-mode
  :straight t
  :when (featurep 'feat/tree-sitter)
  :config
  (add-to-list 'treesit-language-source-alist '(devicetree "https://github.com/joelspadin/tree-sitter-devicetree"))
  (treesit-ensure-installed 'devicetree))


;; Major mode for Device Tree Binary (`*.dtb') files
(use-package virtual-dts-mode
  :straight (:host github :repo "connorfeeley/virtual-dts-mode"))


(provide 'on-demand/me-devicetree)
;;; me-devicetree.el ends here
