;;; me-devicetree.el --- Devicetree support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-extra-mode 'me-devicetree '(dts-mode virtual-dts-mode)
  :auto-mode '(("\\.dtsi?\\'" . dts-mode)
               (".+\\.dtb\\|dtbo\\'" . virtual-dts-mode)))

(use-package dts-mode
  :straight t)

(use-package virtual-dts-mode
  :straight (:host github :repo "connorfeeley/virtual-dts-mode"))


(provide 'modes/me-devicetree)
;;; me-devicetree.el ends here
