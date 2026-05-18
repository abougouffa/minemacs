;;; me-docker.el --- Docker extra stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-10
;; Last modified: 2026-05-18

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-docker
  :auto-mode `((,(rx (or (and (any "/\\") (or "Dockerfile" "Containerfile" "Podmanfile")
                              (? "." (* nonl)))
                         (and "." (or (and (any "Dd") "ocker") (and (any "Pp") "odman")) "file"))
                     eos)
                . dockerfile-mode)
               ("docker-compose[^/]*\\.ya?ml\\'" . docker-compose-mode)
               ("/Apptainer\\(?:[/]*\\)?\\'" . apptainer-mode)))


;; Major mode for editing Docker's Dockerfiles
(use-package dockerfile-mode
  :straight t)


;; Major mode for editing docker-compose files
(use-package docker-compose-mode
  :straight t)


;; Major mode for Apptainer definition files
(use-package apptainer-mode
  :straight (:host github :repo "jrgant/apptainer-mode")
  :config
  (setq apptainer-boxed-headers t
        apptainer-boxed-sections t))


(provide 'on-demand/me-docker)
;;; me-docker.el ends here
