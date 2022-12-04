;;; me-flycheck-cmake.el --- flycheck integration for CMake-based projects

;;; Commentary:

;;; Code:

(require 'json)

(defvar-local +flycheck-cmake-json nil)
(defvar-local +flycheck-cmake-compiler nil)

(defun +flycheck-cmake-clear-cache ()
  "Clear cached compiler and \"compile_commands.json\" data."
  (interactive)
  (setq-local +flycheck-cmake-json nil
              +flycheck-cmake-compiler nil))

(defun +flycheck-cmake-extract-args (&optinal filename)
  "Extract compiler arguments for FILENAME.
When FILENAME is nil, use the file name of the current buffer."
  (let ((file (replace-regexp-in-string
               ;; TODO: Check if the file exists, if not try with ".c" instead of ".cpp"
               (rx (seq "." (or "h" "hpp" "hh") eol)) ".cpp"
               (file-truename (or filename (buffer-file-name))))))
    (when (and (derived-mode-p 'c++-mode)
               (string-match
                (file-truename (project-root (project-current))) file))
      (unless +flycheck-cmake-json
        (setq-local +flycheck-cmake-json
                    (json-read-file
                     (expand-file-name
                      "compile_commands.json"
                      (project-root (project-current))))))
      (let* ((js +flycheck-cmake-json)
             (matched-entry (cl-find-if
                             (lambda (entry)
                               (equal (file-truename (cdr (assq 'file entry))) file))
                             js))
             (cmd (cdr (assq 'command matched-entry)))
             (cmake-args ""))
        (when cmd
          (let ((lst (split-string (replace-regexp-in-string " +-o .*\\|(\\|)" "" cmd))))
            (unless +flycheck-cmake-compiler
              (setq-local +flycheck-cmake-compiler (car lst)))
            (setq cmake-args (cdr lst))))))))

(defun +flycheck-cmake--setup (&rest _)
  "Setup GCC and Clang checkers with the right arguments."
  (let ((args (+flycheck-cmake-extract-args)))
    (setq-local flycheck-gcc-args args
                flycheck-clang-args args)))

(advice-add 'flycheck-mode :before #'+flycheck-cmake--setup)

(provide 'me-flycheck-cmake)

;;; me-flycheck-cmake.el ends here
