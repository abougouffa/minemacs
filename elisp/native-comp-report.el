;;; native-comp-report.el --- Native-comp report for packages -*- lexical-binding: t; -*-

(require 'tabulated-list)

(defun +native-comp-report--numeric-sorter (col)
  "Return a predicate sorting `tabulated-list' entries numerically on COL."
  (lambda (a b)
    (< (string-to-number (aref (cadr a) col))
       (string-to-number (aref (cadr b) col)))))

(defun +native-comp-report--entries ()
  "Build the `tabulated-list-entries' for the native-comp report.
Each entry's ID is the absolute file name from `load-history'."
  (let (entries)
    (dolist (entry load-history)
      (when-let* ((file (car entry))
                  ((stringp file))
                  ((file-in-directory-p file package-user-dir))
                  ((not (string-suffix-p "-autoloads.el" file))))
        (let ((counts (list 0 0 0 0)))
          (dolist (item (cdr entry))
            (when-let* (((eq (car-safe item) 'defun))
                        (sym (cdr item))
                        ((fboundp sym))
                        (fn (symbol-function sym)))
              (cl-incf (nth (cond ((native-comp-function-p fn) 0)
                                  ((byte-code-function-p fn) 1)
                                  ((interpreted-function-p fn) 2)
                                  (t 3))
                            counts))))
          (pcase-let ((`(,native ,bytecode ,interp ,other) counts))
            (push (list file
                        (vector (file-relative-name file package-user-dir)
                                (number-to-string native)
                                (number-to-string bytecode)
                                (number-to-string interp)
                                (number-to-string other)
                                (cond ((and (zerop native) (> bytecode 0))
                                       (propertize "not native" 'face 'warning))
                                      ((> interp 0)
                                       (propertize "not compiled" 'face 'error))
                                      (t ""))))
                  entries)))))
    entries))

(defun +native-comp-report-visit ()
  "Visit the source of the file at point (the .el if it exists)."
  (interactive)
  (when-let* ((file (tabulated-list-get-id)))
    (let ((el (concat (file-name-sans-extension file) ".el")))
      (find-file-other-window (if (file-exists-p el) el file)))))

(defvar-keymap +native-comp-report-mode-map
  :parent tabulated-list-mode-map
  "RET" #'+native-comp-report-visit)

(define-derived-mode +native-comp-report-mode tabulated-list-mode
  "Native-Comp-Report"
  "Major mode listing how functions of loaded package files are compiled."
  (setq tabulated-list-format
        `[("File" 60 t)
          ("Native" 7 ,(+native-comp-report--numeric-sorter 1) :right-align t)
          ("Byte-code" 9 ,(+native-comp-report--numeric-sorter 2) :right-align t)
          ("Interpreted" 11 ,(+native-comp-report--numeric-sorter 3) :right-align t)
          ("Other" 6 ,(+native-comp-report--numeric-sorter 4) :right-align t :pad-right 2)
          ("Status" 0 t)])
  ;; A function here means `g' (revert) recomputes the data automatically.
  (setq tabulated-list-entries #'+native-comp-report--entries)
  (setq tabulated-list-sort-key '("File"))
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

;;;###autoload
(defun +native-comp-report ()
  "Report how the functions of the loaded package files are compiled.
Only files actually loaded are considered (autoload stubs of packages not
loaded yet are ignored). Columns: natively compiled, byte-compiled,
interpreted (not compiled at all) and other (macros, aliases, etc.).

In the report buffer: \\<+native-comp-report-mode-map>\\[revert-buffer] refreshes, \
\\[tabulated-list-sort] sorts by the column at point, \\[+native-comp-report-visit] visits the file."
  (interactive)
  (with-current-buffer (get-buffer-create "*native-comp report*")
    (+native-comp-report-mode)
    (tabulated-list-print)
    (display-buffer (current-buffer))))

(provide 'native-comp-report)

;;; native-comp-report.el ends here
