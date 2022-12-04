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

(defun +flycheck-cmake-get-compile-db-file (&optional root)
  "Get the \"compile_commands.json\" file starting from ROOT path.
When ROOT is nil, use the project root."
  (catch 'file-found
    (let ((proj-root (or root (project-root (project-current)))))
      (dolist (file '("compile_commands.json"
                      "build/compile_commands.json"
                      "build/release/compile_commands.json"
                      "build/debug/compile_commands.json"))
        (let ((full-file-name (expand-file-name file proj-root)))
          (when (file-exists-p full-file-name)
            (throw 'file-found full-file-name)))))))

(defun +flycheck-cmake-extract-args (&optional filename)
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
        (when-let ((db-file (+flycheck-cmake-get-compile-db-file)))
          (setq-local +flycheck-cmake-json (json-read-file db-file))))
      (let* ((matched-entry (cl-find-if
                             (lambda (entry)
                               (equal (file-truename (cdr (assq 'file entry))) file))
                             +flycheck-cmake-json))
             (cmd (cdr (assq 'command matched-entry)))
             (cmake-args ""))
        (when cmd
          (let ((lst (split-string (replace-regexp-in-string " +-o .*\\|(\\|)" "" cmd))))
            (unless +flycheck-cmake-compiler
              (setq-local +flycheck-cmake-compiler (car lst)))
            (setq cmake-args (cdr lst))))))))

(defun +flycheck-cmake--setup (&rest _)
  "Setup GCC and Clang checkers with the right arguments."
  (when (derived-mode-p 'c-mode 'c-ts-mode 'c++-mode 'c++-ts-mode)
    (ignore-errors
      (let ((args (+flycheck-cmake-extract-args)))
        (setq-local flycheck-gcc-args args
                    flycheck-clang-args args)))))

(advice-add 'flycheck-mode :before #'+flycheck-cmake--setup)

(provide 'me-flycheck-cmake)

;;; me-flycheck-cmake.el ends here
