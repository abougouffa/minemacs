;; -*- lexical-binding: t; -*-

;;;###autoload
(defun +git-toplevel (&rest segments)
  "Return the path to the current repo's root."
  (cl-destructuring-bind (code . output)
      (call-process "git" nil nil nil "rev-parse" "--show-toplevel")
    (if (zerop code)
        (apply #'file-name-concat output segments)
      ;; TODO throw stderr as error
      (user-error "Not in a git repo: %s" default-directory))))
