;; -*- lexical-binding: t; -*-

;;;###autoload
(defmacro me-log! (msg &rest vars)
  "Log MSG and VARS using `message' when `minemacs-verbose' is non-nil."
  `(when minemacs-verbose
    (apply #'message (list (concat "[MinEmacs] " ,msg) ,@vars))))

;;;###autoload
(defmacro me-info! (msg &rest vars)
  "Log info MSG and VARS using `message'."
  `(let ((inhibit-message t))
    (apply #'message (list (concat "[MinEmacs] " ,msg) ,@vars))))

;; See https://emacs.stackexchange.com/questions/3022/reset-custom-variable-to-default-value-programmatically0
;;;###autoload
(defun me-reset-sym (sym)
  "Reset SYM to its standard value."
  (set sym (eval (car (get sym 'standard-value)))))

;;;###autoload
(defmacro me-with-shutup! (&rest body)
  "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
  `(let ((message-log-max nil))
    (with-temp-message (or (current-message) "") ,@body)))

;;;###autoload
(defmacro me-reset-var! (var)
  "Reset VAR to its standard value."
  `(setq ,var (eval (car (get ',var 'standard-value)))))

;;;###autoload
(defmacro me-cmdfy! (body)
  "Convert BODY to an interactive command."
  `(lambda ()
     (interactive)
     (,@body)))

;;;###autoload
(defun me-set-fonts ()
  (interactive)
  (custom-set-faces
   `(default
     ((t (:font ,(format "%s %d"
                  (or (plist-get me-fonts :font-family)
                   (plist-get me-default-fonts :font-family))
                  (or (plist-get me-fonts :font-size)
                   (plist-get me-default-fonts :font-size)))))))
   `(fixed-pitch
     ((t (:inherit (default)))))
   `(fixed-pitch-serif
     ((t (:inherit (default)))))
   `(variable-pitch
     ((t (:font ,(format "%s %d"
                  (or (plist-get me-fonts :variable-pitch-font-family)
                   (plist-get me-default-fonts :variable-pitch-font-family))
                  (or (plist-get me-fonts :variable-pitch-font-size)
                   (plist-get me-default-fonts :variable-pitch-font-size)))))))))

;;;###autoload
(defun me-plist-keys (plist)
  "Return the keys of PLIST."
  (let (keys)
    (while plist
      (push (car plist) keys)
      (setq plist (cddr plist)))
    keys))

;;;###autoload
(defun me-serialize-symbol (sym dir &optional filename-format)
  "Serialize SYM to DIR.
If FILENAME-FORMAT is non-nil, use it to format the file name (ex. \"file-%s.el\").
Return the written file name, or nil if SYM is not bound."
  (when (boundp sym)
    (let ((out-file (expand-file-name
                     (format (or filename-format "%s.el") (symbol-name sym))
                     dir)))
      (me-log! "Saving `%s' to file \"%s\"" (symbol-name sym) out-file)
      (with-temp-buffer
        (prin1 (eval sym) (current-buffer))
        (me-with-shutup! (write-file out-file)))
      out-file)))

;;;###autoload
(defun me-deserialize-symbol (sym dir &optional mutate filename-format)
  "Deserialize SYM from DIR, if MUTATE is non-nil, assign the object to SYM.
If FILENAME-FORMAT is non-nil, use it to format the file name (ex. \"file-%s.el\").
Return the deserialized object, or nil if the SYM.el file dont exist."
  (let ((in-file (expand-file-name
                  (format (or filename-format "%s.el") (symbol-name sym))
                  dir))
        res)
    (when (file-exists-p in-file)
      (me-log! "Loading `%s' from file \"%s\"" sym in-file)
      (with-temp-buffer
        (insert-file-contents in-file)
        (goto-char (point-min))
        (ignore-errors (setq res (read (current-buffer)))))
      (when mutate (set sym res)))
    res))

;;;###autoload
(defun me-check-dependencies ()
  "Check for MinEmacs dependencies."
  (interactive)
  (let ((buf (get-buffer-create "*minemacs-dependencies*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "-----------------------\n")
      (insert " MinEmacs dependencies \n")
      (insert "-----------------------\n")
      (dolist (dep me-deps-executables)
        (let ((path (executable-find (symbol-name dep))))
          (insert " ⦿ ")
          (insert (propertize (symbol-name dep) 'face (list 'bold (if path 'success 'error))))
          (insert (if path (concat " found at " (propertize path 'face 'shadow)) " not found!"))
          (insert (propertize (if path " [✓]" " [❌]") 'face (list 'bold (if path 'success 'error)))))
        (insert "\n"))))
  (switch-to-buffer "*minemacs-dependencies*"))

;; Adapted from `doom-lib'
;;;###autoload
(defun me-compile-functions (&rest fns)
  "Queue FNS to be byte/natively-compiled after a brief delay."
  (with-memoization (get 'me-compile-function 'timer)
    (run-with-idle-timer
     1.5 t (lambda ()
             (when-let (fn (pop fns))
               (me-info! "compile-functions: %s" fn)
               (or (if (featurep 'native-compile)
                       (or (subr-native-elisp-p (indirect-function fn))
                           (ignore-errors (native-compile fn))))
                   (byte-code-function-p fn)
                   (let (byte-compile-warnings)
                     (byte-compile fn))))
             (unless fns
               (cancel-timer (get 'me-compile-function 'timer))
               (put 'me-compile-function 'timer nil))))))
