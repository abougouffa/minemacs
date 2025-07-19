;;; me-quickrun.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-07-19
;; Last modified: 2025-07-19

;;; Commentary:

;;; Code:


;; Out of the box code execution from editing buffer
(use-package quickrun
  :straight t
  :hook (quickrun--mode . minemacs-reduce-font-size)
  :hook (pet-mode . +pet-quickrun-setup)
  :bind (([f5] . quickrun))
  :config
  ;; BUG+TODO: When the path contains spaces, this will fail to work. Using
  ;; `shell-quote-argument' don't work either.
  (defun +pet-quickrun-setup ()
    (with-eval-after-load 'pet
      (let ((cmd-alist (copy-alist (quickrun--command-info "python"))))
        (dolist (key '(:command :compile-only))
          (let* ((cmd (assq key cmd-alist))
                 (args (string-split (cdr cmd))))
            (setcdr cmd (string-join `(,(pet-executable-find (car args)) ,@(cdr args)) " "))))
        (setq-local quickrun-option-cmd-alist cmd-alist)))))


(provide 'obsolete/me-quickrun)
;;; me-quickrun.el ends here
