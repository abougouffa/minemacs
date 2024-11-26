;;; me-ffip.el --- description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:


(use-package find-file-in-project
  :straight t
  :custom
  (ffip-use-rust-fd (and (executable-find "fd") t))
  :config
  (require 'project) ; for `project--file-completion-table'
  (advice-add ;; This adds `nerd-icons-completion-mode' support for `ffip'
   'ffip-completing-read :override
   (satch-defun +ffip-completing-read (prompt collection &optional action)
     (when-let* ((selected
                  (if (= 1 (length collection))
                      (car collection)
                    (let ((sel (completing-read prompt (project--file-completion-table collection))))
                      (or (assoc sel collection) sel)))))
       (let* ((default-directory (ffip-get-project-root-directory))
              (result (if (consp selected) (cdr selected) selected)))
         (if action (funcall action result) result))))))


(provide 'obsolete/me-ffip)
;;; me-ffip.el ends here
