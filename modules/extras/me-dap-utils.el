;;; me-dap-utils.el --- Utility functions for DAP mode -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>

;;; Commentary:

;;; Code:

;;;###autoload
(defun +github-latest-release (user repo &optional fallback-release)
  "Get the latest release of USER/REPO. Strips the \"v\" at left.

Fallback to FALLBACK-RELEASE when it can't get the last one."
  (if-let ((latest
            (ignore-errors
              (with-temp-buffer
                (url-insert-file-contents
                 (format
                  "https://api.github.com/repos/%s/%s/releases/latest"
                  user repo))
                (json-parse-buffer :object-type 'plist)))))
      (string-trim-left
       (car (last (split-string (plist-get latest :html_url) "/")))
       "v")
    fallback-release))


(provide 'me-dap-utils)

;;; me-dap-utils.el ends here
