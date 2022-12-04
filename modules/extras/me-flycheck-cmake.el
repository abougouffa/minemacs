;;; me-flycheck-cmake.el --- flycheck checker based on cmake

(require 'json)

(defvar-local +flycheck-cmake-json nil)
(defvar-local +flycheck-cmake-compiler nil)

(defun +flycheck-cmake-clear-cache ()
  (setq-local +flycheck-cmake-json nil
              +flycheck-cmake-compiler nil))

(defun +flycheck-cmake-extract-args ()
  (let ((file (file-truename (buffer-file-name))))
    (setq file (replace-regexp-in-string "\.h$" ".cpp" file))
    (when (and (derived-mode-p 'c++-mode)
               (string-match (file-truename (projectile-project-root)) file))
      (unless +flycheck-cmake-json
        (setq-local +flycheck-cmake-json
                    (json-read-file
                     (expand-file-name
                      "compile_commands.json"
                      (projectile-project-root)))))
      (let* ((js +flycheck-cmake-json)
             (matched-entry (cl-find-if
                             (lambda (entry)
                               (equal (file-truename (cdr (assq 'file entry))) file))
                             js))
             (cmd (cdr (assq 'command matched-entry)))
             (cmake-args ""))
        (when cmd
          (let ((lst (split-string (replace-regexp-in-string " +-o .*\\|(\\|)" "" cmd))))
            (setq-local +flycheck-cmake-compiler (car lst))
            (setq cmake-args (cdr lst))))))))

(defun +flycheck-cmake--setup (&rest _)
  (let ((args (+flycheck-cmake-extract-args)))
    (setq flycheck-gcc-args args
          flycheck-clang-args args)))

(advice-add 'flycheck-mode :before #'+flycheck-cmake--setup)

(provide 'me-flycheck-cmake)
;;; flycheck-cmake.el ends here
