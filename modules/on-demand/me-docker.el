;;; me-docker.el --- Docker extra stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-docker
  :auto-mode `((("\\.dockerfile\\'" "[/\\]\\(?:Containerfile\\|Dockerfile\\)\\(?:\\.[^/\\]*\\)?\\'") . dockerfile-mode)
               ("docker-compose[^/]*\\.ya?ml\\'" . docker-compose-mode)))

(use-package dockerfile-mode
  :straight t)

(use-package docker-compose-mode
  :straight t)


(provide 'on-demand/me-docker)
;;; me-docker.el ends here
