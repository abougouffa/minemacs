;; -*- lexical-binding: t; -*-

;;;###autoload
(defmacro +error! (msg &rest vars)
  "Log error MSG and VARS using `message'."
  (when (>= minemacs-msg-level 1)
    `(let ((inhibit-message t))
      (apply #'message (list (concat "[MinEmacs:Error] " ,msg) ,@vars)))))

;;;###autoload
(defmacro +info! (msg &rest vars)
  "Log info MSG and VARS using `message'."
  (when (>= minemacs-msg-level 2)
    `(let ((inhibit-message t))
      (apply #'message (list (concat "[MinEmacs:Info] " ,msg) ,@vars)))))

;;;###autoload
(defmacro +log! (msg &rest vars)
  "Log MSG and VARS using `message' when `minemacs-verbose' is non-nil."
  (when (>= minemacs-msg-level 3)
    `(apply #'message (list (concat "[MinEmacs:Log] " ,msg) ,@vars))))

;;;###autoload
(defmacro +debug! (msg &rest vars)
  "Log error MSG and VARS using `message'."
  (when (>= minemacs-msg-level 4)
    `(let ((inhibit-message t))
      (apply #'message (list (concat "[MinEmacs:Error] " ,msg) ,@vars)))))

;;;###autoload
(defun +emacs-features-p (&rest feats)
  "Is features FEATS are enabled in this Emacs build."
  (+all (mapcar (lambda (feat) (memq feat emacs/features)) feats)))

;;;###autoload
(defmacro +fn-inhibit-messages! (fn &optional no-message-log)
  "Add an advice arount the function FN to suppress messages in echo area.
If NO-MESSAGE-LOG is non-nil, do not print any message to *Messages* buffer."
  (let ((advice-fn (make-symbol (format "+%s--inhibit-messages-a" fn))))
    `(advice-add
      ',fn :around
      (defun ,advice-fn (origfn &rest args)
       (let ((message-log-max (unless ,no-message-log message-log-max)))
        (with-temp-message (or (current-message) "")
         (+log! "Inhibiting messages of %s" ,(symbol-name fn))
         (apply origfn args)))))))

;; See https://emacs.stackexchange.com/questions/3022/reset-custom-variable-to-default-value-programmatically0
;;;###autoload
(defun +reset-sym (sym)
  "Reset SYM to its standard value."
  (set sym (eval (car (get sym 'standard-value)))))

;;;###autoload
(defmacro +reset-var! (var)
  "Reset VAR to its standard value."
  `(setq ,var (eval (car (get ',var 'standard-value)))))

;;;###autoload
(defmacro +shutup! (&rest body)
  "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
  (if (not minemacs-verbose)
      `(let ((message-log-max nil))
        (with-temp-message (or (current-message) "") ,@body))
    `(progn ,@body)))

;;;###autoload
(defmacro +cmdfy! (&rest body)
  "Convert BODY to an interactive command."
  `(lambda ()
     (interactive)
     ,@body))

;;;###autoload
(defun +set-fonts ()
  (interactive)
  ;; TODO: use (font-family-list) to check if the font is available
  (custom-set-faces
   `(default
     ((t (:font ,(format "%s %d"
                  (or (plist-get minemacs-fonts :font-family)
                   (plist-get minemacs-default-fonts :font-family))
                  (or (plist-get minemacs-fonts :font-size)
                   (plist-get minemacs-default-fonts :font-size)))))))
   `(fixed-pitch
     ((t (:inherit (default)))))
   `(fixed-pitch-serif
     ((t (:inherit (default)))))
   `(variable-pitch
     ((t (:font ,(format "%s %d"
                  (or (plist-get minemacs-fonts :variable-pitch-font-family)
                   (plist-get minemacs-default-fonts :variable-pitch-font-family))
                  (or (plist-get minemacs-fonts :variable-pitch-font-size)
                   (plist-get minemacs-default-fonts :variable-pitch-font-size)))))))))

;;;###autoload
(defun +plist-keys (plist)
  "Return the keys of PLIST."
  (let (keys)
    (while plist
      (push (car plist) keys)
      (setq plist (cddr plist)))
    keys))

;;;###autoload
(defmacro +plist-push! (plist &rest key-vals)
  "Push KEY-VALS to PLIST."
  (let ((out (list 'progn)))
    (while (> (length key-vals) 0)
      (let ((key (pop key-vals))
            (val (pop key-vals)))
        (add-to-list
         'out
         `(setq ,plist (plist-put ,plist ,key ,val)) t)))
    out))

;;;###autoload
(defun +plist-combine (&rest plists)
  "Create a single property list from all plists in PLISTS.
Modified from `org-combine-plists'. This supposes the values to be vectors,
and concatenate them."
  (let ((res (copy-sequence (pop plists)))
        prop val plist)
    (while plists
      (setq plist (pop plists))
      (while plist
        (setq prop (pop plist) val (pop plist))
        (setq res (plist-put res prop (vconcat val (plist-get res prop))))))
    res))

;;;###autoload
(defun +plist-delete (plist prop)
  "Delete property PROP from PLIST.
Adapted from `org-plist-delete'."
  (let (p)
    (while plist
      (if (not (eq prop (car plist)))
          (setq p (plist-put p (car plist) (nth 1 plist))))
      (setq plist (cddr plist)))
    p))

;;;###autoload
(defun +serialize-sym (sym dir &optional filename-format)
  "Serialize SYM to DIR.
If FILENAME-FORMAT is non-nil, use it to format the file name (ex. \"file-%s.el\").
Return the written file name, or nil if SYM is not bound."
  (when (boundp sym)
    (let ((out-file (expand-file-name
                     (format (or filename-format "%s.el") (symbol-name sym))
                     dir)))
      (+log! "Saving `%s' to file \"%s\"" (symbol-name sym) (abbreviate-file-name out-file))
      (with-temp-buffer
        (prin1 (eval sym) (current-buffer))
        (+shutup! (write-file out-file)))
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
      (+log! "Loading `%s' from file \"%s\"" sym (abbreviate-file-name in-file))
      (with-temp-buffer
        (insert-file-contents in-file)
        (goto-char (point-min))
        (ignore-errors (setq res (read (current-buffer)))))
      (when mutate (set sym res)))
    res))

;;;###autoload
(defun +add-dependencies (&rest deps)
  (setq minemacs-deps-executables
        (delete-dups
         (append minemacs-deps-executables deps))))

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
      (dolist (dep minemacs-deps-executables)
        (let ((dep (ensure-list dep)))
          (insert (concat " ● " (if (length> dep 1) (concat (string-join (mapcar #'symbol-name dep) ", ") "\n") "")))
          (dolist (d (ensure-list dep))
            (let ((path (executable-find (symbol-name d))))
              (insert (concat (if (length> dep 1) "   ⦿ " "") (propertize (symbol-name d) 'face (list 'bold (if path 'success 'error)))))
              (insert (if path (concat " found at " (propertize path 'face 'shadow)) " not found!"))
              (insert (propertize (if path " [✓]" " [⨯]") 'face (list 'bold (if path 'success 'error))))
              (insert "\n")))))))
  (switch-to-buffer "*minemacs-dependencies*"))

;; An internal variable to keep track of the tasks
(defvar +eval-when-idle--task-num 0)
(defvar +eval-when-idle-delay 10.0) ;; 10 seconds

;;;###autoload
(defun +eval-when-idle (delay &rest fns)
  "Queue FNS to be processed when Emacs becomes idle."
  (let* ((task-num (cl-incf +eval-when-idle--task-num))
         (task-name (make-symbol (format "+eval-when-idle--task%d" task-num))))
    (with-memoization (get task-name 'timer)
      (run-with-idle-timer
       delay t
       (lambda ()
         (when-let (fn (pop fns))
           (+log! "Running task %d, calling function `%s'"
                  task-num
                  (truncate-string-to-width
                   (format "%s" fn) 40 nil nil "…"))
           (funcall fn))
         (unless fns
           (cancel-timer (get task-name 'timer))
           (put task-name 'timer nil)))))))

;;;###autoload
(defmacro +eval-when-idle! (&rest body)
  "Evaluate BODY when Emacs becomes idle."
  (declare (indent 0))
  `(+eval-when-idle ,+eval-when-idle-delay
    (lambda ()
      ,@body)))

;;;###autoload
(defmacro +eval-when-idle-for! (delay &rest body)
  "Evaluate BODY after DELAY seconds from Emacs becoming idle."
  (declare (indent 1))
  `(+eval-when-idle ,delay
    (lambda ()
      ,@body)))

;; Adapted from `evil-unquote', takes functions into account
;;;###autoload
(defun +unquote (exp)
  "Return EXP unquoted."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

;; Adapted from github.com/d12frosted/environment
;;;###autoload
(defmacro +hook-with-delay! (hook secs function &optional depth local)
  "Add the FUNCTION to the value of HOOK.
The FUNCTION is delayed to be evaluated in SECS once HOOK is
triggered.
DEPTH and LOCAL are passed as is to `add-hook'."
  (let* ((f-name (make-symbol (format "%s-on-%s-delayed-%ds-h" (+unquote function) (+unquote hook) secs)))
         (f-doc (format "Call `%s' in %d seconds" (symbol-name (+unquote function)) secs)))
    `(progn
       (eval-when-compile
         (defun ,f-name () ,f-doc
          (run-with-idle-timer ,secs nil ,function))
         (add-hook ,hook #',f-name ,depth ,local)))))

;; Adapted from `doom-lib'
;;;###autoload
(defun +compile-functs (&rest fns)
  "Queue FNS to be byte/natively-compiled after a brief delay."
  (dolist (fn fns)
    (+eval-when-idle!
      (or (and (featurep 'native-compile)
               (or (subr-native-elisp-p (indirect-function fn))
                   (+shutup!
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
          (insert
           (format
            "\n;; Helper function\n%s\n"
            '(defun +add-to-path (path)
              (unless (member path exec-path)
               (add-to-list 'exec-path path)))))
          (insert "\n;; Adding PATH content to `exec-path'\n")
          (dolist (path (parse-colon-path var-val))
            (when path
              (insert
               (format
                "(+add-to-path \"%s\")\n"
                path path))))
          (insert "\n"))
        (insert
         (format "(setenv \"%s\" \"%s\")\n" env-var var-val))))
    (write-file (concat minemacs-local-dir "system-env.el"))))

;;;###autoload
(defun +env-load ()
  (interactive)
  (let ((env-file (concat minemacs-local-dir "system-env.el")))
    (when (file-exists-p env-file)
      (load env-file (not minemacs-verbose) (not minemacs-verbose)))))
