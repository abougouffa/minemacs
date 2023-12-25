;;; me-spell-fu.el --- Better integraion of spell-fu -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(message "me-spell-fu is deprecated")

;;;###autoload
(defmacro +spell-fu-register-dictionaries! (&rest langs)
  "Register dictionaries for `LANGS` to spell-fu's multi-dict."
  (with-eval-after-load 'spell-fu
    (let* ((fn-name (intern (format "+spell-fu--multi-langs-%s-h" (string-join langs "-"))))
           (closure `(defun ,fn-name ())))
      (dolist (lang langs)
        (setq closure (append closure `((+spell-fu--add-dictionary ,lang)))))
      (append '(add-hook (quote spell-fu-mode-hook)) (list closure)))))

(make-obsolete '+spell-fu-register-dictionaries! 'jinx-mode "2023-12-25")


(provide 'me-spell-fu)

;;; me-spell-fu.el ends here
