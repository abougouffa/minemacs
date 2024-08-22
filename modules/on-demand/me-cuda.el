;;; me-cuda.el --- CUDA integration -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-cuda
  :auto-mode '(("\\.cu[h]?\\'" . cuda-mode)))

(use-package cuda-mode
  :straight t
  :hook (cuda-mode . +prog-mode-run-hooks))


(provide 'on-demand/me-cuda)
;;; me-cuda.el ends here
