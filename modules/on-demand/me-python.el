;;; me-python.el --- Python extra packages -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-python
  :companion-packages '(((python-mode python-ts-mode) . python-docstring-mode))
  :auto-mode '((("requirements\\.in" "\\.pip\\'" "requirements[^z-a]*\\.txt\\'") . pip-requirements-mode)))


;; Smart Python docstring formatting
(use-package python-docstring
  :ensure t
  :hook ((python-mode python-ts-mode) . python-docstring-mode))


;; Helpers to run Python's pytest
(use-package python-pytest
  :ensure t)


;; Major mode for editing Python's pip requirements files
(use-package pip-requirements
  :ensure t)


(provide 'on-demand/me-python)
;;; me-python.el ends here
