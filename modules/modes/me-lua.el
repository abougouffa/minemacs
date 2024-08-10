;;; me-lua.el --- Lua support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-extra-mode 'me-lua 'lua-mode
  :auto-mode '(("\\.lua\\'" . lua-mode))
  :interpreter-mode '(("lua" . lua-mode)))

(use-package lua-mode
  :straight t
  :custom
  (lua-indent-level 2))


(provide 'modes/me-lua)
;;; me-lua.el ends here
