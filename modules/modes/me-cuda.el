;;; me-cuda.el --- CUDA integration -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-extra-mode 'me-cuda 'cuda-mode
  :auto-mode '(("\\.cu[h]?\\'" . cuda-mode)))

(use-package cuda-mode
  :straight t
  :hook (cuda-mode . display-line-numbers-mode)
  :hook (cuda-mode . hs-minor-mode))


(provide 'modes/me-cuda)
;;; me-cuda.el ends here
