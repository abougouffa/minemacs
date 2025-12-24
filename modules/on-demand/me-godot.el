;;; me-godot.el --- Godot script language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-22
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-godot
  :auto-mode '(("\\.gd\\'" . gdscript-mode) ("\\.tres\\'" . conf-toml-mode) ("\\.tscn\\'" . conf-toml-mode)))


;; Major mode for Godot's GDScript language
(use-package gdscript-mode
  :ensure t)


(provide 'on-demand/me-godot)
;;; me-godot.el ends here
