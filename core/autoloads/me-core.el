;; -*- lexical-binding: t; -*-

;;;###autoload
(defun me-log! (msg &rest vars)
  "Log MSG and VARS using `message' when `init-file-debug' is non-nil."
  (when init-file-debug
    (apply #'message (cons (concat "[MinEmacs] " msg) vars))))

;;;###autoload
(defun me-info! (msg &rest vars)
  "Log info MSG and VARS using `message'."
  (let ((inhibit-message))
    (apply #'message (cons (concat "[MinEmacs] " msg) vars))))

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
