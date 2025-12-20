;;; me-arduino.el --- Arduino support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-10
;; Last modified: 2025-12-20

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-arduino
  :auto-mode '((("\\.ino\\'" "\\.pde\\'") . +arduino-mode)))

(if (featurep 'feat/tree-sitter)
    (define-derived-mode +arduino-mode c++-ts-mode "Arduino" "Alias for `c++-ts-mode' to edit Arduino files.")
  (define-derived-mode +arduino-mode c++-mode "Arduino" "Alias for `c++-mode' to edit Arduino files."))

(add-hook 'auto-mode-alist '("\\.\\(ino\\|pde\\)\\'" . +arduino-mode))


;; Arduino CLI command wrapper
(use-package arduino-cli-mode
  :straight t
  :hook +arduino-mode
  :custom
  (arduino-cli-verify t)
  :config
  (defun +arduino-cli-list-all-boards ()
    (let ((lines (string-lines (shell-command-to-string "arduino-cli board listall")))
          (regexp (rx (group "Board Name" (+ space)) "FQBN"))
          first-len boards)
      (dolist (line lines)
        (cond ((string-match regexp line)
               (setq first-len (length (match-string 1 line))))
              ((and first-len (length> line 1))
               (push (cons (string-trim (substring line 0 first-len)) (substring line first-len)) boards))))
      boards))

  (defvar +arduino-cli-boards nil)
  (defun +arduino-cli-boards ()
    (with-memoization +arduino-cli-boards (+arduino-cli-list-all-boards)))

  (with-eval-after-load 'marginalia ; Register the annonater for Marginalia
    (defun +marginalia-annotate-arduino-cli-boards-template (cand)
      (when-let* ((fqbn (alist-get cand (+arduino-cli-boards) nil nil 'equal)))
        (marginalia--fields (fqbn :face 'marginalia-size))))
    (add-to-list 'marginalia-annotators '(arduino-board +marginalia-annotate-arduino-cli-boards-template builtin none)))

  (defun +arduino-cli-set-board (dirlocal-p)
    "Set the board FQBN for the current buffer (or directory when run with prefix)."
    (interactive "P")
    (when-let* ((board (completing-read "Choose a template: " (+completion-mark-category (+arduino-cli-boards) 'arduino-board)))
                (fqbn (alist-get board (+arduino-cli-boards) nil nil 'equal))
                (port (serial-read-name)))
      (if dirlocal-p
          (progn
            (add-dir-local-variable '+arduino-mode 'arduino-cli-default-fqbn fqbn)
            (add-dir-local-variable '+arduino-mode 'arduino-cli-default-port port))
        (add-file-local-variable-prop-line 'arduino-cli-default-fqbn fqbn)
        (add-file-local-variable-prop-line 'arduino-cli-default-port port)))))


(provide 'on-demand/me-arduino)
;;; me-arduino.el ends here
