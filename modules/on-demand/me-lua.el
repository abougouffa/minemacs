;;; me-lua.el --- Lua support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-lua
  :auto-mode '(("\\.lua\\'" . lua-mode))
  :interpreter-mode '(("lua" . lua-mode)))


;; Major mode for editing Lua scripts
(use-package lua-mode
  :straight t
  :custom
  (lua-indent-level 2))


(provide 'on-demand/me-lua)
;;; me-lua.el ends here
