;;; me-project-x.el --- Extensions for the builtin `project' -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-06-26
;; Last modified: 2025-06-26

;;; Commentary:

;;; Code:

;;;###autoload(with-eval-after-load 'project (require 'me-project-x))

(defcustom +fd-program (if (executable-find "fdfind") "fdfind" "fd")
  "The fd program to use."
  :group 'minemacs-project
  :type 'string)

(defun +project--fd-ignores-arguments (ignores dir)
  "Like `xref--find-ignores-arguments', but for \"fd\"."
  (cl-assert (not (string-match-p "\\`~" dir)))
  (if ignores
      (concat " -E " (mapconcat #'shell-quote-argument ignores " -E "))
    ""))

(defun +project--files-in-directory-fd (dir ignores &optional files)
  "Like `project--files-in-directory', but for \"fd\"."
  (let* ((dir (file-name-as-directory dir))
         (default-directory dir)
         ;; Make sure ~/ etc. in local directory name is expanded and not left
         ;; for the shell command to interpret.
         (localdir (file-name-unquote (file-local-name (expand-file-name dir))))
         (command (format "%s -H %s -t f -0 %s"
                          +fd-program
                          (+project--fd-ignores-arguments ignores "./")
                          (if files
                              (concat (shell-quote-argument "(")
                                      (shell-quote-argument
                                       (mapconcat
                                        (lambda (wildcard)
                                          (thread-last
                                            (wildcard-to-regexp wildcard)
                                            (string-remove-suffix "\\'")
                                            (string-remove-prefix "\\`")))
                                        (split-string files)
                                        (concat "|")))
                                      (shell-quote-argument ")"))
                            "")))
         res)
    (with-temp-buffer
      (let ((status
             (process-file-shell-command command nil t))
            (pt (point-min)))
        (unless (zerop status)
          (goto-char (point-min))
          (if (and
               (not (eql status 127))
               (search-forward "Permission denied\n" nil t))
              (let ((end (1- (point))))
                (re-search-backward "\\`\\|\0")
                (error "File listing failed: %s"
                       (buffer-substring (1+ (point)) end)))
            (error "File listing failed: %s" (buffer-string))))
        (goto-char pt)
        (while (search-forward "\0" nil t)
          (push (buffer-substring-no-properties (+ pt 2) (1- (point)))
                res)
          (setq pt (point)))))
    (if project-files-relative-names
        (sort res #'string<)
      (project--remote-file-names
       (mapcar (lambda (s) (concat localdir s))
               (sort res #'string<))))))

(when (executable-find +fd-program)
  (advice-add 'project--files-in-directory :override '+project--files-in-directory-fd))


(provide 'me-project-x)
;;; me-project-x.el ends here
