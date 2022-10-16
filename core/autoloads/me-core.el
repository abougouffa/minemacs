;; -*- lexical-binding: t; -*-

;;;###autoload
(defmacro +log! (msg &rest vars)
  "Log MSG and VARS using `message' when `minemacs-verbose' is non-nil."
  (when minemacs-verbose
    `(apply #'message (list (concat "[MinEmacs] " ,msg) ,@vars))))

;;;###autoload
(defmacro +info! (msg &rest vars)
  "Log info MSG and VARS using `message'."
  `(let ((inhibit-message t))
    (apply #'message (list (concat "[MinEmacs] " ,msg) ,@vars))))

;; See https://emacs.stackexchange.com/questions/3022/reset-custom-variable-to-default-value-programmatically0
;;;###autoload
(defun +reset-sym (sym)
  "Reset SYM to its standard value."
  (set sym (eval (car (get sym 'standard-value)))))

;;;###autoload
(defmacro +with-shutup! (&rest body)
  "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
  `(let ((message-log-max nil))
    (with-temp-message (or (current-message) "") ,@body)))

;;;###autoload
(defmacro +reset-var! (var)
  "Reset VAR to its standard value."
  `(setq ,var (eval (car (get ',var 'standard-value)))))

;;;###autoload
(defmacro +cmdfy! (body)
  "Convert BODY to an interactive command."
  `(lambda ()
     (interactive)
     (,@body)))

;;;###autoload
(defun +set-fonts ()
  (interactive)
  ;; TODO: use (font-family-list) to check if the font is available
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
(defun +plist-keys (plist)
  "Return the keys of PLIST."
  (let (keys)
    (while plist
      (push (car plist) keys)
      (setq plist (cddr plist)))
    keys))

;;;###autoload
(defun +serialize-sym (sym dir &optional filename-format)
  "Serialize SYM to DIR.
If FILENAME-FORMAT is non-nil, use it to format the file name (ex. \"file-%s.el\").
Return the written file name, or nil if SYM is not bound."
  (when (boundp sym)
    (let ((out-file (expand-file-name
                     (format (or filename-format "%s.el") (symbol-name sym))
                     dir)))
      (+log! "Saving `%s' to file \"%s\"" (symbol-name sym) out-file)
      (with-temp-buffer
        (prin1 (eval sym) (current-buffer))
        (+with-shutup! (write-file out-file)))
      out-file)))

;;;###autoload
(defun +deserialize-sym (sym dir &optional mutate filename-format)
  "Deserialize SYM from DIR, if MUTATE is non-nil, assign the object to SYM.
If FILENAME-FORMAT is non-nil, use it to format the file name (ex. \"file-%s.el\").
Return the deserialized object, or nil if the SYM.el file dont exist."
  (let ((in-file (expand-file-name
                  (format (or filename-format "%s.el") (symbol-name sym))
                  dir))
        res)
    (when (file-exists-p in-file)
      (+log! "Loading `%s' from file \"%s\"" sym in-file)
      (with-temp-buffer
        (insert-file-contents in-file)
        (goto-char (point-min))
        (ignore-errors (setq res (read (current-buffer)))))
      (when mutate (set sym res)))
    res))

;;;###autoload
(defun +check-dependencies ()
  "Check for MinEmacs dependencies."
  (interactive)
  (let ((buf (get-buffer-create "*minemacs-dependencies*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "-----------------------\n")
      (insert " MinEmacs dependencies \n")
      (insert "-----------------------\n")
      (dolist (dep me-deps-executables)
        (let ((dep (ensure-list dep)))
          (insert (concat " ⦿ " (if (length> dep 1) (concat (+str-join ", " (mapcar #'symbol-name dep)) "\n") "")))
          (dolist (d (ensure-list dep))
            (let ((path (executable-find (symbol-name d))))
              (insert (concat (if (length> dep 1) "   ● " "") (propertize (symbol-name d) 'face (list 'bold (if path 'success 'error)))))
              (insert (if path (concat " found at " (propertize path 'face 'shadow)) " not found!"))
              (insert (propertize (if path " [✓]" " [⨯]") 'face (list 'bold (if path 'success 'error))))
              (insert "\n")))))))
  (switch-to-buffer "*minemacs-dependencies*"))

;; An internal variable to keep track of the tasks
(defvar me--eval-when-idle-task 0)

;;;###autoload
(defun +eval-when-idle (&rest fns)
  "Queue FNS to be processed when Emacs becomes idle."
  (let* ((task-num (atomic-change-group
                     (setq me--eval-when-idle-task (1+ me--eval-when-idle-task))))
         (task-name (make-symbol (format "me--do-when-idle-task%d" task-num))))
    (with-memoization (get task-name 'timer)
      (run-with-idle-timer
       1.5 t
       (lambda ()
         (when-let (fn (pop fns))
           (+log! "Running task %d, calling function `%s'"
                    task-num
                    (truncate-string-to-width
                     (format "%s" fn) 40 nil nil "..."))
           (funcall fn))
         (unless fns
           (cancel-timer (get task-name 'timer))
           (put task-name 'timer nil)))))))

;;;###autoload
(defmacro +eval-when-idle! (&rest body)
  "Push BODY to be processed when Emacs becomes idle."
  `(+eval-when-idle
    (lambda ()
      ,@body)))

;; Adapted from `doom-lib'
;;;###autoload
(defun +compile-functs (&rest fns)
  "Queue FNS to be byte/natively-compiled after a brief delay."
  (dolist (fn fns)
    (+eval-when-idle!
     (or (and (featurep 'native-compile)
              (or (subr-native-elisp-p (indirect-function fn))
                  (+with-shutup!
                   (ignore-errors (native-compile fn)))))
         (byte-code-function-p fn)
         (let (byte-compile-warnings)
           (byte-compile fn))))))

;;;###autoload
(defun +env-save ()
  (interactive)
  (with-temp-buffer
    (insert ";; -*- mode: emacs-lisp; -*-\n\n")
    (dolist (env-var +env-save-vars)
      (when-let ((var-val (getenv env-var)))
        (when (equal "PATH" env-var)
          (insert "\n;; Adding PATH content to `exec-path'\n")
          (dolist (path (parse-colon-path var-val))
            (when path
              (insert
               (format
                "(unless (member \"%s\" exec-path) (add-to-list 'exec-path \"%s\"))\n"
                path path))))
          (insert "\n"))
        (insert
         (format "(setenv \"%s\" \"%s\")\n" env-var var-val))))
    (write-file (expand-file-name "env" minemacs-local-dir))))

;;;###autoload
(defun +env-load ()
  (interactive)
  (let ((env-file (expand-file-name "env" minemacs-local-dir)))
    (when (file-exists-p env-file))
    (with-temp-buffer
      (insert-file env-file)
      (eval-buffer))))

;;;###autoload
(defun me-update ()
  (interactive)
  (straight-pull-all)
  (straight-check-all))
