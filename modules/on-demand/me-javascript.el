;;; me-javascript.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-08-30
;; Last modified: 2025-08-30

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-javascript
  :companion-packages '(((js-mode js-ts-mode typescript-mode typescript-ts-mode) . add-node-modules-path)))


;; Adds the "node_modules/.bin" directory to the buffer "exec_path"
(use-package add-node-modules-path
  :straight t
  :hook (js-base-mode . add-node-modules-path)
  :config
  (when (executable-find "pnpm")
    (setopt add-node-modules-path-command '("pnpm bin" "pnpm bin -w"))))


(provide 'on-demand/me-javascript)
;;; me-javascript.el ends here
